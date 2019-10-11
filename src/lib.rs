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
use embedded_hal::blocking::delay::{DelayMs, DelayUs};
use hd44780_driver::{
    Cursor,
    DataBus,
    Display,
    DisplayMode,
    HD44780,
};

use heapless::{
    consts::U10,
    consts::U18,
    consts::U20,
    String,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keys {
    None,
    Enter,
    NextMenu,
    PreviousMenu,
    NextItem,
    PreviousItem,
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
    ($hd44780:expr, $($arg:tt)*) => ({
        let mut output: String<U20> = String::new();
        if core::fmt::write(&mut output, format_args!($($arg)*)).is_ok() {

            #[cfg(test)] { print!("{}", output); }

            for b in output.chars() {
                $hd44780.write_char(b);
            }
        }
    })
}

pub struct Menu<'a, Context> {
    pub name: &'a str,
    pub show: [Option<&'a MenuItem<'a, Context> >; 4],
    pub menu: &'a [&'a MenuItem<'a, Context>],
}

pub enum WriteOptions {
    Next,
    Previous
}

type WriteCallbackFn<C> = fn(wo: WriteOptions, context: &mut C);
type ReadCallbackFn<C> = fn(buf: &mut dyn Write, context: &C);

#[allow(dead_code)]
pub enum MenuItemType<'a, Context> {
    SubMenu(&'a [MenuItemType<'a, Context>]),
    ReadValue(ReadCallbackFn<Context>),
    WriteValue(ReadCallbackFn<Context>, WriteCallbackFn<Context>),
}

pub struct MenuItem<'a, Context> {
    pub name: &'a str,
    pub parent: Option<&'a MenuItem<'a, Context> >,
    pub hint: Option<&'a str>,
    pub menu_type: MenuItemType<'a, Context>,
}
impl<'a, Context> MenuItem<'a, Context> {
    fn to_string(&self, ctx: &mut Context) -> String<U20> {
        let mut output: String<U20> = String::new();

        match self.menu_type {
            MenuItemType::SubMenu(..) => {
                let _ = write!(output, "{}", self.name);
            },
            MenuItemType::ReadValue(ref rcb) | MenuItemType::WriteValue(ref rcb, ..)  => {
                let mut string = String::<U10>::new();
                rcb(&mut string, ctx);
                if let Some(hint) = self.hint {
                    let _ = write!(output, "{}: {} {}", self.name, string, hint);
                }
                else {
                    let _ = write!(output, "{}: {}", self.name, string);
                }
            },
        }

        output
    }
}

#[allow(dead_code)]
#[derive(Clone, Copy)]
enum MenuState<'a, Context> {
    Show,
    BrowseMenus(&'a [&'a MenuItem<'a, Context>], usize),
    ChangeSetting(&'a MenuItem<'a, Context>),
}

#[allow(dead_code)]
pub struct Dispatcher<'a, Context> {
    state: MenuState<'a, Context>,
    menu: &'a Menu<'a, Context>,
}

#[allow(dead_code)]
impl<'a, Context> Dispatcher<'a, Context> {
    pub const fn new(menu: &'a Menu<'_, Context>) -> Self {
        Dispatcher {
            state: MenuState::Show,
            menu,
        }
    }

    fn calc_window(min: usize, idx: usize, max: usize, size: usize) -> (usize, usize) {
        assert!(min < idx && idx < max, "calc_window: parameter error!");

        if idx +2 >= max {
            (max - size, max)
        }
        else if idx.saturating_sub(1) < min {
            (min, min +size)
        }
        else {
            (idx -1, min + size)
        }
    }

    pub fn reset_to_show(&mut self) {
        self.state = MenuState::Show;
    }

    pub fn run<D,B>(&mut self, keys: &dyn ContainsKey, ctx: &mut Context, drv: &mut HD44780<D,B>) where D: DelayMs<u8> + DelayUs<u16>, B: DataBus {
        self.state = match self.state {
            MenuState::Show => {
                //drv.clear();
                //drv.reset();
                for (r, menuitem) in self.menu.show.iter().enumerate() {
                    if let Some(menuitem) = menuitem {
                        let s = menuitem.to_string(ctx);

                        drv.set_cursor_pos(ROW_START[r]);
                        vprint!(drv, "{}", s);
                        for _ in 0 .. (20 - s.len() ) {
                            drv.write_char(' ');
                        }
                    }
                }
                if keys.contains(Keys::None) {
                    MenuState::Show
                }
                else {
                    MenuState::BrowseMenus(self.menu.menu, 0)
                }
            },
            MenuState::BrowseMenus(r, mut idx) => {
                if idx >= r.len() { idx = r.len()-1; }

                drv.clear();
                drv.reset();

                let size = if r.len() < LINES { r.len() } else { LINES };
                let (min, max) = Self::calc_window(0, idx, r.len(), size);

                for (r, submenu) in self.menu.menu.iter().skip(min).take(max).enumerate() {
                    let s = submenu.to_string(ctx);

                    drv.set_cursor_pos(ROW_START[r]);
                    vprint!(drv, "  {}", s);
                }

                drv.set_cursor_pos(ROW_START[idx - min]);
                drv.write_char('>');

                if keys.contains(Keys::NextMenu) {
                    MenuState::BrowseMenus(r, idx.saturating_sub(1) )
                }
                else if keys.contains(Keys::NextMenu) {
                    MenuState::BrowseMenus(r, idx.saturating_add(1) )
                }
                else if keys.contains(Keys::Enter) {
                    MenuState::ChangeSetting(r[idx])
                }
                else {
                    MenuState::BrowseMenus(r, idx)
                }
            },
            MenuState::ChangeSetting(r) => {
                MenuState::ChangeSetting(r)
            },
        }
    }
}
