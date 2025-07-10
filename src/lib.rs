//! # print-run
//!
//! A procedural macro crate for tracing Rust function and module execution — with style.
//!
//! `print-run` provides `#[print_run]` attribute macro that automatically logs when functions start and finish, with optional color, duration, nested indentation, timestamps and more.
//!
//! It’s ideal for skeletons, debugging, quick prototypes, educational examples, or even just fun visual feedback.
//!
//! ## Features
//!
//! - Supports standalone functions, `impl` blocks, and inline `mod`s
//! - Logs entry and exit of functions
//! - Draws visual call hierarchy (indents nested calls)
//! - Measures and displays execution duration
//! - Supports async functions
//! - Optional color and timestamps
//! - `msg!()` macro for indented, bold `println!`-style output
//!
//! ## Usage
//!
//! Apply `#[print_run]` to any function:
//!
//! ```rust
//! use print_run::print_run;
//!
//! #[print_run]
//! fn demo() {
//!     msg!("Running...");
//! }
//! ```
//!
//! Use `#[print_run_defaults(...)]` to set global options for the crate:
//!
//! ```rust
//! #[print_run_defaults(indent, colored)]
//! ```
//!
//! #### For more, see the crate [README](https://github.com/HeyJ0e/print-run-rs).
//!
//! ## License
//!
//! Licensed under MIT.

use nu_ansi_term::{Color, ansi::RESET};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{ToTokens, format_ident, quote};
use std::sync::{Once, OnceLock};
use syn::{
    Attribute, Error, FnArg, Ident, ImplItem, ImplItemFn, Item, ItemFn, ItemImpl, ItemMod, LitStr,
    Result, Token, parse, parse_macro_input, parse_quote,
};

#[allow(unused)]
#[cfg(doctest)]
mod helper;

static IS_HELPER_MODULE_ADDED: Once = Once::new();
static PRINT_RUN_DEFAULTS: OnceLock<PrintRunArgs> = OnceLock::new();

/// Optional input flags
#[derive(Clone, Debug, Default)]
struct PrintRunArgs {
    colored: Option<bool>,
    duration: Option<bool>,
    indent: Option<bool>,
    skip: Option<bool>,
    supress_labels: Option<bool>,
    timestamps: Option<bool>,
    __struct_prefix: Option<String>,
}

impl PrintRunArgs {
    fn merge(&mut self, override_args: &PrintRunArgs) {
        self.colored = override_args.colored.or(self.colored);
        self.duration = override_args.duration.or(self.duration);
        self.indent = override_args.indent.or(self.indent);
        self.skip = override_args.skip.or(self.skip);
        self.supress_labels = override_args.supress_labels.or(self.supress_labels);
        self.timestamps = override_args.timestamps.or(self.timestamps);
        self.__struct_prefix = override_args
            .__struct_prefix
            .clone()
            .or_else(|| self.__struct_prefix.clone());
    }

    fn add_globals(&mut self) {
        if let Some(glob) = get_print_run_defaults() {
            if let Some(v) = glob.colored {
                self.colored = Some(v);
            }
            if let Some(v) = glob.duration {
                self.duration = Some(v);
            }
            if let Some(v) = glob.indent {
                self.indent = Some(v);
            }
            if let Some(v) = glob.skip {
                self.skip = Some(v);
            }
            if let Some(v) = glob.supress_labels {
                self.supress_labels = Some(v);
            }
            if let Some(v) = glob.timestamps {
                self.timestamps = Some(v);
            }
        }
    }

    fn to_attribute(&self) -> Attribute {
        let arg_idents = self.to_idents();
        let pre = self.__struct_prefix.as_ref().and_then(|p| Some(p.as_str()));

        match (arg_idents.is_empty(), pre) {
            (true, None) => parse_quote! {
                #[print_run::print_run]
            },
            (true, Some(pre_val)) => parse_quote! {
                #[print_run::print_run(__struct_prefix = #pre_val)]
            },
            (false, None) => parse_quote! {
                #[print_run::print_run( #(#arg_idents),* )]
            },
            (false, Some(pre_val)) => parse_quote! {
                #[print_run::print_run( #(#arg_idents),*, __struct_prefix = #pre_val )]
            },
        }
    }

    fn to_idents(&self) -> Vec<Ident> {
        let mut result = Vec::new();

        if self.colored == Some(true) {
            result.push(format_ident!("colored"));
        }
        if self.duration == Some(true) {
            result.push(format_ident!("duration"));
        }
        if self.indent == Some(true) {
            result.push(format_ident!("indent"));
        }
        if self.skip == Some(true) {
            result.push(format_ident!("skip"));
        }
        if self.supress_labels == Some(true) {
            result.push(format_ident!("supress_labels"));
        }
        if self.timestamps == Some(true) {
            result.push(format_ident!("timestamps"));
        }

        result
    }
}

impl parse::Parse for PrintRunArgs {
    fn parse(input: parse::ParseStream) -> Result<Self> {
        let mut args = PrintRunArgs::default();

        while !input.is_empty() {
            let ident: Ident = input.parse()?;
            let _ = input.parse::<Option<Token![,]>>(); // allow optional commas

            match ident.to_string().as_str() {
                "colored" => args.colored = Some(true),
                "duration" => args.duration = Some(true),
                "indent" => args.indent = Some(true),
                "skip" => args.skip = Some(true),
                "supress_labels" => args.supress_labels = Some(true),
                "timestamps" => args.timestamps = Some(true),
                "__struct_prefix" => {
                    let _ = input.parse::<Option<Token![=]>>()?;
                    let lit: LitStr = input.parse()?;
                    args.__struct_prefix = Some(lit.value().to_string());
                }
                other => {
                    return Err(Error::new(
                        ident.span(),
                        format!("Unknown attribute '{}'", other),
                    ));
                }
            }
        }

        Ok(args)
    }
}

macro_rules! or_else {
    ($cond:expr, $true_:expr, $false_:expr) => {
        if $cond { $true_ } else { $false_ }
    };
}

macro_rules! or_nothing {
    ($cond:expr, $true_:expr) => {
        or_else!($cond, $true_, quote! {})
    };
}

macro_rules! or_empty_str {
    ($cond:expr, $true_:expr) => {
        or_else!($cond, $true_, quote! { || "".to_string() })
    };
}

macro_rules! colorize {
    ($txt:expr, $col_name: ident) => {
        format!("{}{}{}", Color::$col_name.prefix().to_string(), $txt, RESET)
    };
}

macro_rules! colorize_fn {
    ($color_name: ident) => {{
        let color = Color::$color_name.prefix().to_string();
        quote! {
            |txt: String| format!("{}{}{}", #color, txt, #RESET)
        }
    }};
    ($color_name: ident, "bold") => {{
        let color = Color::$color_name.bold().prefix().to_string();
        quote! {
            |txt: String| format!("{}{}{}", #color, txt, #RESET)
        }
    }};
}

macro_rules! create_timestamp {
    ($colored:expr) => {{
        let colorize = or_else!(
            $colored,
            colorize_fn!(DarkGray),
            quote! { |txt: String| txt }
        );
        quote! {
            || {
                let now = std::time::SystemTime::now();
                let epoch = now
                    .duration_since(std::time::UNIX_EPOCH)
                    .expect("Time went backwards");

                let total_secs = epoch.as_secs();
                let millis = epoch.subsec_millis();

                let hours = (total_secs / 3600) % 24;
                let minutes = (total_secs / 60) % 60;
                let seconds = total_secs % 60;
                let ts = format!("{:02}:{:02}:{:02}.{:03}", hours, minutes, seconds, millis);
                let ts = {#colorize}(ts);
                format!("{} ", ts) // don't colorize the separator
            }
        }
    }};
}

macro_rules! create_duration {
    ($colored:expr, $supress_labels:expr) => {{
        let colorize = or_else!($colored, colorize_fn!(Green), quote! { |txt: String| txt });
        let supress_labels = $supress_labels;
        quote! {
            |start: std::time::Instant| {
                let elapsed = start.elapsed().as_nanos();
                let dur =
                    if elapsed < 1_000 {
                        format!("{}ns", elapsed)
                    } else if elapsed < 1_000_000 {
                        format!("{:.2}µs", elapsed as f64 / 1_000.0)
                    } else if elapsed < 1_000_000_000 {
                        format!("{:.2}ms", elapsed as f64 / 1_000_000.0)
                    } else {
                        format!("{:.2}s", elapsed as f64 / 1_000_000_000.0)
                    }
                ;
                let dur = {#colorize}(dur); // colorize the result only
                if #supress_labels {
                    format!("[{}]", dur)
                } else {
                    format!(" in {}", dur)
                }
            }
        }
    }};
}

macro_rules! create_indent {
    ($val:expr, $ch:expr) => {{
        let val = $val;
        let ch = $ch;

        // hack for doctest
        let depth_path: syn::Expr = if cfg!(doctest) {
            syn::parse_str("crate::helper::DEPTH").unwrap()
        } else {
            syn::parse_str("crate::__print_run_helper::DEPTH").unwrap()
        };

        quote! {
            || {#depth_path}.with(|depth| {
                let depth_val = *depth.borrow();
                *depth.borrow_mut() = depth_val.saturating_add_signed(#val);
                let depth_val= depth_val.saturating_add_signed((#val-1) / 2);
                let spaces = "┆ ".repeat(depth_val);
                format!("{}{} ", spaces, #ch)
            })
        }
    }};
}

fn get_print_run_defaults() -> Option<&'static PrintRunArgs> {
    PRINT_RUN_DEFAULTS.get()
}

/// Attribute macro to define global defaults for `#[print_run(...)]`.
///
/// This macro sets the default configuration for all `#[print_run]` invocations
/// in the current crate. It can be overridden locally.
///
/// It allows you to avoid repeating common arguments like `colored`, `indent`, or `duration`
/// across multiple annotated items.
///
/// > ⚠️ Due to current Rust limitations, this cannot be written as a crate-level attribute like `#![print_run_defaults(...)]`.
/// > Instead, place it somewhere inside the crate as a regular attribute.
///
/// # Usage
///
/// ```rust
/// use print_run::{print_run, print_run_defaults};
///
/// #[print_run_defaults(colored, indent, duration)]
/// #[print_run] // inherits: colored, indent, duration
/// pub fn foo() {
///     msg!("Hello from one");
/// }
///
/// #[print_run(timestamps)] // adds timestamps to colored, indent, duration
/// pub fn bar() {
///     msg!("Hello from two");
/// }
///
/// # fn main() {}
/// ```
///
/// # Available arguments
///
/// - `colored`: Enable ANSI color output
/// - `duration`: Show elapsed time on function exit
/// - `indent`: Show visual indentation based on call depth
/// - `skip`: Skip instrumentation for the entire crate
/// - `supress_labels`: Hide `starting` / `ended` labels
/// - `timestamps`: Add timestamp to each printed line
///
/// # Notes
///
/// - Can be used **once per create** (typically at the top of a module)
/// - Only applies to functions that annotated with `#[print_run(...)]`
/// - Cannot be applied with `#![...]` syntax (Rust limitation)
///
/// # See also
///
/// - [`print_run`](macro@crate::print_run): For instrumenting individual functions/modules/impl blocks
///
#[proc_macro_attribute]
pub fn print_run_defaults(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr_clone = attr.clone();
    let args = parse_macro_input!(attr as PrintRunArgs);

    if PRINT_RUN_DEFAULTS.set(args).is_err() {
        return Error::new_spanned(
            TokenStream2::from(attr_clone),
            "print run defaults already set",
        )
        .to_compile_error()
        .into();
    }

    input
}

/// Procedural macro to automatically log function entry and exit.
///
/// This macro prints messages when a function or method begins and ends execution.
/// It can display timestamps, durations, and indentation based on configuration flags.
/// It also enables a `msg!()` macro inside annotated functions for indented logging.
///
/// # Features
///
/// - Logs function start and end messages
/// - Supports optional ANSI color output
/// - Shows nested indentation for call hierarchy
/// - Supports async functions
/// - Shows execution duration (auto scales to s/ms/µs/ns)
/// - Optionally prints timestamps
/// - Usable on functions, impl blocks, and inline modules
///
/// # Usage
///
/// ```
/// use print_run::print_run;
///
/// #[print_run]
/// fn my_func() {
///     msg!("Doing something!");
/// }
///
/// #[print_run(colored, duration, indent)]
/// fn another() { }
///
/// # fn main() {}
/// ```
///
/// # Available arguments
///
/// - `colored`: Enable ANSI color output
/// - `duration`: Show elapsed time on function exit
/// - `indent`: Show visual indentation based on call depth
/// - `skip`: Skip instrumentation for this item
/// - `supress_labels`: Hide `starting` / `ended` labels
/// - `timestamps`: Add timestamp to each printed line
///
/// # Module-level usage
///
/// Can be used on inline `mod` blocks to apply the same instrumentation to all functions inside:
///
/// ```
/// use print_run::print_run;
///
/// #[print_run(indent, duration)]
/// mod my_mod {
///     pub fn foo() {}
///
///     #[print_run(skip)]
///     pub fn bar() {} // This will not be instrumented
/// }
///
/// # fn main() {}
/// ```
///
/// # Impl blocks
///
/// Apply to `impl` blocks to instrument all methods inside:
///
/// ```
/// use print_run::print_run;
///
/// struct MyStruct;
///
/// #[print_run(indent)]
/// impl MyStruct {
///     fn method_1(&self) {}
///     fn method_2(&self) {}
/// }
///
/// # fn main() {}
/// ```
///
/// # `msg!()` macro
///
/// Inside `#[print_run]` functions, you can use `msg!()` as an indented alternative to `println!()`.
/// It aligns with current indentation and can help make logs clearer:
///
/// ```
/// use print_run::print_run;
///
/// #[print_run(indent)]
/// fn do_something() {
///     msg!("Hello {}", 123);
/// }
///
/// # fn main() {}
/// ```
///
/// # Limitations
///
/// - Only **inline modules** (`mod mymod { ... }`) are supported
/// - Procedural macros can't modify items across files
/// - Crate-level defaults must be written as `#[print_run_defaults(...)]` (not `#![...]`)
///
#[proc_macro_attribute]
pub fn print_run(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as PrintRunArgs);

    // Try parsing as a function
    if let Ok(mut func) = parse::<ItemFn>(item.clone()) {
        let new_args = extract_and_flatten_print_args(&args, &mut func.attrs);
        return print_run_fn(new_args, func);
    }

    // Try parsing as a module
    if let Ok(mut module) = parse::<ItemMod>(item.clone()) {
        let new_args = extract_and_flatten_print_args(&args, &mut module.attrs);
        return print_run_mod(new_args, module);
    }

    // Try parsing as an implementation
    if let Ok(mut implementation) = parse::<ItemImpl>(item.clone()) {
        let new_args = extract_and_flatten_print_args(&args, &mut implementation.attrs);
        return print_run_impl(new_args, implementation);
    }

    // Unsupported item — return error
    Error::new_spanned(
        TokenStream2::from(item),
        "#[print_run] can only be used on functions, implementations or inline modules",
    )
    .to_compile_error()
    .into()
}

fn print_run_fn(mut args: PrintRunArgs, mut fn_item: ItemFn) -> TokenStream {
    // Add global args
    args.add_globals();

    // Extract args and item attributes
    let PrintRunArgs {
        colored,
        duration,
        indent,
        skip,
        supress_labels,
        timestamps,
        __struct_prefix,
    } = args;
    let colored = colored == Some(true);
    let duration = duration == Some(true);
    let indent = indent == Some(true);
    let skip = skip == Some(true);
    let supress_labels = supress_labels == Some(true);
    let timestamps = timestamps == Some(true);

    if skip {
        // Add use println as msg to prevent missing `msg` macro
        let use_msg = parse_quote! { use std::println as msg; };
        fn_item.block.stmts.insert(0, use_msg);
        return fn_item.to_token_stream().into();
    }

    let ItemFn {
        attrs,
        vis,
        sig,
        block,
    } = fn_item;

    // Create name with prefix
    let fn_name = sig.ident.to_string();
    let prefix = __struct_prefix.unwrap_or("".into());
    let fn_name = format!("{prefix}{fn_name}");

    // Create labels
    let start_label = or_else!(!supress_labels, "starting", "");
    let end_label = or_else!(!supress_labels, "ended", "");

    // Create start/end function names
    let start = or_else!(colored, colorize!(fn_name.clone(), Yellow), fn_name.clone());
    let end = or_else!(colored, colorize!(fn_name.clone(), Blue), fn_name.clone());

    // Create timestamp creator closure
    let create_timestamp_fn = or_empty_str!(timestamps, create_timestamp!(colored));

    // Create duration creator closure
    let duration_fn = or_else!(
        duration,
        create_duration!(colored, supress_labels),
        quote! { |_| "".to_string() }
    );

    // Create indent creator closures
    let indent_top = or_empty_str!(indent, create_indent!(1isize, "┌"));
    let indent_bottom = or_empty_str!(indent, create_indent!(-1isize, "└"));
    let indent_body = or_empty_str!(indent, create_indent!(0isize, ""));

    // Create msg! macro
    let colorize_msg = if colored {
        colorize_fn!(White, "bold")
    } else {
        colorize_fn!(Default, "bold")
    };
    let msg_macro = quote! {
        #[allow(unused)]
        macro_rules! msg {
            ($($arg:tt)*) => {{
                let ts = {#create_timestamp_fn}();
                let indent = {#indent_body}();
                let msg = format!($($arg)*);
                let msg = {#colorize_msg}(msg);
                println!("{}{} {}", ts, indent, msg);
            }};
        }
    };

    // Wrap the original function body
    let new_block = quote! {
        {
            let ts = {#create_timestamp_fn}();
            let start = std::time::Instant::now();
            let indent = {#indent_top}();
            println!("{}{}{} {}", ts, indent, #start, #start_label);
            #msg_macro

            let result = {#block};

            let dur = {#duration_fn}(start);
            let ts = {#create_timestamp_fn}();
            let indent = {#indent_bottom}();
            println!("{}{}{} {}{}", ts, indent, #end, #end_label, dur);
            result
        }
    };

    // Add helper module if needed
    let helper_module = define_helper_module();

    // Reconstruct the function
    quote! {
        #(#attrs)*
        #vis #sig #new_block
        #helper_module
    }
    .into()
}

fn print_run_mod(args: PrintRunArgs, mut module_item: ItemMod) -> TokenStream {
    // Check if it's an inline module
    let content = match module_item.content {
        Some((_, ref mut items)) => items,
        _ => {
            return Error::new_spanned(
                module_item.mod_token,
                "`#[print_run]` only supports inline modules",
            )
            .to_compile_error()
            .into();
        }
    };

    // Add the macro attribute to all functions and struct methods in the module
    for item in content {
        match item {
            // Normal functions
            Item::Fn(func) => {
                let new_args = extract_and_flatten_print_args(&args, &mut func.attrs);
                func.attrs.push(new_args.to_attribute());
            }
            // Struct member functions
            Item::Impl(item_impl) => {
                let new_args = extract_and_flatten_print_args(&args, &mut item_impl.attrs);
                item_impl.attrs.push(new_args.to_attribute());
            }
            _ => {}
        }
    }

    // Add helper module if needed
    let helper_module = define_helper_module();
    let module_tokens = module_item.into_token_stream();

    quote! { #module_tokens #helper_module }.into()
}

fn print_run_impl(args: PrintRunArgs, mut impl_item: ItemImpl) -> TokenStream {
    // Get struct name
    let ty_str = (&impl_item.self_ty).into_token_stream().to_string();

    // Look for methods
    for impl_item in &mut impl_item.items {
        if let ImplItem::Fn(method) = impl_item {
            let mut new_args = extract_and_flatten_print_args(&args, &mut method.attrs);
            let is_static = is_static_method(&method);
            let ty_str = ty_str.clone() + if is_static { "::" } else { "." };
            new_args.__struct_prefix = Some(ty_str);
            method.attrs.push(new_args.to_attribute());
        }
    }

    // Add helper module if needed
    let helper_module = define_helper_module();
    let impl_tokens = impl_item.into_token_stream();

    quote! { #impl_tokens #helper_module }.into()
}

fn extract_and_flatten_print_args(
    parent: &PrintRunArgs,
    attrs: &mut Vec<Attribute>,
) -> PrintRunArgs {
    // Get global defaults or empty args
    let mut merged_args = get_print_run_defaults()
        .as_deref()
        .and_then(|a| Some(a.clone()))
        .unwrap_or(PrintRunArgs::default());

    // Keep only non-#[print_run] attrs
    attrs.retain(|attr| {
        if attr.path().is_ident("print_run") {
            if let Ok(parsed) = attr.parse_args::<PrintRunArgs>() {
                merged_args.merge(&parsed);
            }
            false // drop this attr
        } else {
            true // keep
        }
    });
    merged_args.merge(parent);
    merged_args.add_globals();

    merged_args
}

fn define_helper_module() -> TokenStream2 {
    // Define support module with DEPTH if it runs for the first time
    let mut define = false;
    IS_HELPER_MODULE_ADDED.call_once(|| define = true);
    or_nothing!(
        define,
        quote! {
            #[doc(hidden)]
            #[allow(unused)]
            pub(crate) mod __print_run_helper {
                use std::cell::RefCell;
                thread_local! {
                    pub static DEPTH: RefCell<usize> = RefCell::new(0);
                }
            }
        }
    )
}

fn is_static_method(method: &ImplItemFn) -> bool {
    match method.sig.inputs.first() {
        Some(FnArg::Receiver(_)) => false, // instance method
        Some(FnArg::Typed(_)) => true,     // static method (no self)
        None => true,                      // no args = static
    }
}
