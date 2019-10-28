//! Embedded Menu System

#![no_std]

#![deny(
    nonstandard_style,
    rust_2018_idioms,
    future_incompatible,
    unused_extern_crates,
    unused_import_braces,
    unused_qualifications,
    unused_results,
    //warnings,
    //unused,
    unsafe_code,
)]
#![warn(
    trivial_casts,
    trivial_numeric_casts,
    clippy::all,
    clippy::pedantic,
    clippy::nursery,
    clippy::wildcard_dependencies
)]
#![allow(clippy::integer_arithmetic)]
#![allow(clippy::multiple_crate_versions)]
#![allow(clippy::toplevel_ref_arg)]
#![allow(clippy::print_stdout)]

use core::fmt::Write;
use hd44780_driver::HD44780;

use heapless::{
    consts::U12,
    consts::U20,
    String,
    Vec,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keys {
    None,
    Ignore,
    Enter,
    NextMenu,
    PreviousMenu,
    NextItem,
    PreviousItem,
    Next,
    Back,
}

pub trait ContainsKey {
    fn contains(&self, k: Keys) -> bool;
}

impl ContainsKey for (Keys,) {
    fn contains(&self, k: Keys) -> bool {
        self.0 == k
    }
}
impl ContainsKey for (Keys,Keys,) {
    fn contains(&self, k: Keys) -> bool {
        self.0 == k || self.1 == k
    }
}
impl ContainsKey for (Keys,Keys,Keys,) {
    fn contains(&self, k: Keys) -> bool {
        self.0 == k || self.1 == k || self.2 == k
    }
}
impl ContainsKey for (Keys,Keys,Keys,Keys,) {
    fn contains(&self, k: Keys) -> bool {
        self.0 == k || self.1 == k || self.2 == k || self.3 == k
    }
}
impl ContainsKey for (Keys,Keys,Keys,Keys,Keys,) {
    fn contains(&self, k: Keys) -> bool {
        self.0 == k || self.1 == k || self.2 == k || self.3 == k || self.4 == k
    }
}

const LINES: usize = 4;
const ROW_START: [u8; LINES] = [
    0x0, 0x40, 0x14, 0x54,
];

/// Try to print to the hd44780 driver
#[macro_export]
macro_rules! vprint {
    ($hd44780:expr, $size:ident, $($arg:tt)*) => ({
        let mut output: String<$size> = String::new();
        if core::fmt::write(&mut output, format_args!($($arg)*)).is_ok() {
            for b in output.chars() {
                $hd44780.write_char(b);
            }
            for _ in 0..output.capacity() - output.len() {
                $hd44780.write_char(' ');
            }
        }
    })
}

#[macro_export]
macro_rules! vprintln {
    ($stdout:expr)                         => { vprint!($stdout, U20, "") };
    ($stdout:expr, $fmt:expr)              => { vprint!($stdout, U20, $fmt) };
    ($stdout:expr, $fmt:expr, $($arg:tt)*) => { vprint!($stdout, U20, $fmt, $($arg)*) };
}

pub struct Menu<'a, Context> {
    pub name: &'a str,
    pub show: &'a MenuItem<'a, Context>,
    pub menu: &'a MenuItem<'a, Context>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WriteOptions {
    Next,
    Previous
}

type ActiveCallbackFn<C> = fn(context: &C) -> bool;
type WriteCallbackFn<C> = fn(wo: WriteOptions, context: &mut C);
type ReadCallbackFn<C> = fn(buf: &mut dyn Write, context: &C);
type FullScreenCallbackFn<C> = fn(drv: &mut dyn HD44780, context: &C, );
type ExecCallbackFn<C> = fn(context: &mut C);

#[allow(dead_code)]
pub enum MenuItemType<'a, Context> {
    SubMenu(&'a [&'a MenuItem<'a, Context>], ActiveCallbackFn<Context>),
    FullScreen(FullScreenCallbackFn<Context>),
    ReadValue(ReadCallbackFn<Context>),
    WriteValue(ReadCallbackFn<Context>, WriteCallbackFn<Context>),
    ExecValue(ReadCallbackFn<Context>, ExecCallbackFn<Context>),
}

pub struct MenuItem<'a, Context> {
    pub short_name: &'a str,
    pub long_name: Option<&'a str>,
    pub parent: Option<&'a MenuItem<'a, Context> >,
    pub menu_type: MenuItemType<'a, Context>,
}
impl<'a, Context> MenuItem<'a, Context> {
    fn to_string(&self, ctx: &mut Context) -> String<U20> {
        let mut output: String<U20> = String::new();

        match self.menu_type {
            MenuItemType::SubMenu(.., acb) => {
                if acb(ctx) {
                    let _ = write!(output, "{}", self.short_name);
                }
                else {
                    let _ = write!(output, "[{}]", self.short_name);
                }
            }
            MenuItemType::FullScreen(..) => {
                let _ = write!(output, "{}", self.short_name);
            },
            MenuItemType::ReadValue(ref rcb) | MenuItemType::WriteValue(ref rcb, ..) | MenuItemType::ExecValue(ref rcb, ..)  => {
                let mut string = String::<U12>::new();
                rcb(&mut string, ctx);
                let _ = write!(output, "{}: {}", self.short_name, string);
            },
        }

        output
    }
    fn to_display(&self, ctx: &mut Context) -> String<U20> {
        let mut output: String<U20> = String::new();

        match self.menu_type {
            MenuItemType::SubMenu(.., acb) => {
                if acb(ctx) {
                    let _ = write!(output, "{}", self.short_name);
                }
                else {
                    let _ = write!(output, "[{}]", self.short_name);
                }
            }
            MenuItemType::FullScreen(..) => {
                let _ = write!(output, "{}", self.short_name);
            },
            MenuItemType::ReadValue(ref rcb) | MenuItemType::WriteValue(ref rcb, ..) | MenuItemType::ExecValue(ref rcb, ..)  => {
                let mut string = String::<U20>::new();
                rcb(&mut string, ctx);
                let _ = write!(output, "{}", string);
            },
        }

        output
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
enum MenuState<'a, Context> {
    BrowseMenus(&'a MenuItem<'a, Context>, usize),
    ChangeSetting(&'a MenuItem<'a, Context>),
    Show(&'a MenuItem<'a, Context>),
}

#[allow(dead_code)]
pub struct Dispatcher<'a, Context> {
    state: MenuState<'a, Context>,
    menu: &'a Menu<'a, Context>,
    change: bool,
    previous_idxs: Vec<usize, U12>,
}

#[allow(dead_code)]
impl<'a, Context> Dispatcher<'a, Context> {
    pub fn new(menu: &'a Menu<'_, Context>) -> Self {
        Dispatcher {
            change: true,
            state: MenuState::BrowseMenus(menu.show, 0),
            menu,
            previous_idxs: Vec::new(),
        }
    }

    fn calc_window(min: usize, idx: usize, max: usize, size: usize) -> (usize, usize) {
        assert!(min <= idx && idx <= max, "calc_window: parameter error!");

        if idx +2 >= max {
            (max - size, max)
        }
        else if idx.saturating_sub(2) <= min {
            (min, min +size)
        }
        else {
            (idx -2, min + size)
        }
    }

    pub fn reset_to_show(&mut self) {
        self.state = MenuState::BrowseMenus(self.menu.show, 0);
    }

    pub fn handle_input(&mut self, keys: &dyn ContainsKey, ctx: &mut Context) {
        if keys.contains(Keys::None) {
            self.change = false;
        }
        else {
            self.change = true;
        }

        self.state = match self.state {
            MenuState::BrowseMenus(r, mut idx) => {
                if let MenuItemType::SubMenu(list, acb)  = r.menu_type {
                    if keys.contains(Keys::Back) || !acb(ctx) {
                        let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                            index
                        }
                        else {
                            0
                        };
                        MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                    }
                    else if keys.contains(Keys::NextMenu) {
                        MenuState::BrowseMenus(r, idx.saturating_add(1) )
                    }
                    else if keys.contains(Keys::PreviousMenu) {
                        MenuState::BrowseMenus(r, idx.saturating_sub(1) )
                    }
                    else if keys.contains(Keys::Enter) || keys.contains(Keys::Next) {
                        if self.previous_idxs.push(idx).is_err() {
                            let _ = self.previous_idxs.pop();
                            let _ = self.previous_idxs.push(idx);
                        }
                        MenuState::BrowseMenus(list[idx], 0)
                    }
                    else {
                        MenuState::BrowseMenus(r, idx)
                    }
                }
                else if let MenuItemType::FullScreen(_)  = r.menu_type {
                    MenuState::Show(r)
                }
                else if let MenuItemType::ReadValue(_)  = r.menu_type {
                    MenuState::Show(r)
                }
                else if let MenuItemType::WriteValue(_, _)  = r.menu_type {
                    MenuState::ChangeSetting(r)
                }
                else {
                    if let MenuItemType::ExecValue(_, ecb)  = r.menu_type {
                        ecb(ctx);
                    }

                    let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                        index
                    }
                    else {
                        0
                    };
                    MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                }
            },
            MenuState::Show(s) => {
                if keys.contains(Keys::None) {
                    MenuState::Show(s)
                }
                else {
                    let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                        index
                    }
                    else {
                        0
                    };
                    MenuState::BrowseMenus(s.parent.unwrap_or(self.menu.menu), prev_idx)
                }
            },
            MenuState::ChangeSetting(r) => {
                if let MenuItemType::WriteValue(_, fcb) = r.menu_type {
                    if keys.contains(Keys::NextItem) {
                        fcb(WriteOptions::Next, ctx);
                        MenuState::ChangeSetting(r)
                    }
                    else if keys.contains(Keys::PreviousItem) {
                        fcb(WriteOptions::Previous, ctx);
                        MenuState::ChangeSetting(r)
                    }
                    else {
                        if keys.contains(Keys::Enter) || keys.contains(Keys::Back) {
                            let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                                index
                            }
                            else {
                                0
                            };
                            MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                        }
                        else {
                            MenuState::ChangeSetting(r)
                        }
                    }
                }
                else {
                    let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                        index
                    }
                    else {
                        0
                    };
                    MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                }
            },
        }
    }

    pub fn run(&mut self, ctx: &mut Context, drv: &mut dyn HD44780) {
        /* Reset Display, unless told not to, which should happen in the default case. */
        if self.change {
            drv.clear();

            /* To simplify the key handling code, it needs two passes when a change has been made.
             * This is the second pass.
             * */
            self.handle_input( &(Keys::None,), ctx);
            self.change = false;
        }

        self.state = match self.state {
            MenuState::BrowseMenus(r, mut idx) => {
                // TODO: add back to previous menu
                if let MenuItemType::SubMenu(list, _)  = r.menu_type {
                    if list.len() > 0 {
                        if idx >= list.len() { idx = 0; }

                        let size = if list.len() < LINES { list.len() } else { LINES };
                        let (min, max) = Self::calc_window(0, idx, list.len(), size);

                        let mut max_idx = 0;
                        for (r, submenu) in list.iter().skip(min).take(max).enumerate() {
                            let s = submenu.to_string(ctx);

                            drv.set_cursor_pos(ROW_START[r]);
                            vprintln!(drv, "  {}", s);

                            max_idx = r;
                        }

                        drv.set_cursor_pos(ROW_START[idx - min]);
                        drv.write_char('>');
                    }
                    else {
                        drv.set_cursor_pos(ROW_START[0]);
                        vprintln!(drv, "<Empty>",);
                    }
                    MenuState::BrowseMenus(r, idx)
                }
                else if let MenuItemType::FullScreen(_)  = r.menu_type {
                    MenuState::Show(r)
                }
                else if let MenuItemType::ReadValue(_)  = r.menu_type {
                    MenuState::Show(r)
                }
                else if let MenuItemType::WriteValue(_, _)  = r.menu_type {
                    MenuState::ChangeSetting(r)
                }
                else {
                    if let MenuItemType::ExecValue(_, ecb)  = r.menu_type {
                        ecb(ctx);
                    }

                    let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                        index
                    }
                    else {
                        0
                    };
                    MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                }
            },
            MenuState::Show(s) => {
                if let MenuItemType::FullScreen(fcb) = s.menu_type {
                    fcb(drv, ctx);
                }
                else if let MenuItemType::ReadValue(_) = s.menu_type {
                    drv.set_cursor_pos(ROW_START[0]);
                    vprintln!(drv);

                    drv.set_cursor_pos(ROW_START[1]);
                    if let Some(name) = s.long_name {
                        vprintln!(drv, "  {}", name);
                    }
                    else {
                        vprintln!(drv, "  {}", s.short_name);
                    }

                    drv.set_cursor_pos(ROW_START[2]);
                    vprintln!(drv, "  {}", s.to_display(ctx) );

                    drv.set_cursor_pos(ROW_START[3]);
                    vprintln!(drv);
                }
                MenuState::Show(s)
            },
            MenuState::ChangeSetting(r) => {
                if let MenuItemType::WriteValue(_, _) = r.menu_type {
                    drv.set_cursor_pos(ROW_START[0]);
                    vprintln!(drv);

                    drv.set_cursor_pos(ROW_START[1]);
                    if let Some(name) = r.long_name {
                        vprintln!(drv, "  {}", name);
                    }
                    else {
                        vprintln!(drv, "  {}", r.short_name);
                    }


                    drv.set_cursor_pos(ROW_START[2]);
                    vprintln!(drv, "  {}", r.to_display(ctx) );

                    drv.set_cursor_pos(ROW_START[3]);
                    vprintln!(drv);

                    MenuState::ChangeSetting(r)
                }
                else {
                    let prev_idx = if let Some(index) = self.previous_idxs.pop() {
                        index
                    }
                    else {
                        0
                    };
                    MenuState::BrowseMenus(r.parent.unwrap_or(self.menu.menu), prev_idx)
                }
            },
        }
    }
}
