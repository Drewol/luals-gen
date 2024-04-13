use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput, LitStr};

#[proc_macro_derive(ToLuaLsType)]
pub fn my_macro(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let serde = input.attrs.iter().find(|x| x.path().is_ident("serde"));
    let mut rename_all = None;
    if let Some(serde) = serde {
        _ = serde.parse_nested_meta(|meta| {
            if meta.path.is_ident("rename_all") {
                let v = meta.value()?;
                let v: LitStr = v.parse()?;
                if let Some(case) = CASES.iter().find(|x| x.0 == v.value()) {
                    rename_all = Some(case.1);
                }
            }

            Ok(())
        });
    }

    let expanded = match &input.data {
        syn::Data::Struct(syn::DataStruct {
            struct_token: _,
            fields,
            semi_token: _,
        }) => {
            let items = fields.iter().enumerate().map(|(i, f)| {
                let lua_index = format!("[{}]", i + 1);
                let id = match &f.ident {
                    Some(id) => id.to_string(),
                    None => lua_index,
                };

                let mut id = if let Some(case) = rename_all {
                    convert_case::Converter::new().to_case(case).convert(id)
                } else {
                    id
                };

                f.attrs
                    .iter()
                    .find(|x| x.path().is_ident("serde"))
                    .and_then(|x| {
                        x.parse_nested_meta(|x| {
                            if x.path.is_ident("rename") {
                                let v = x.value()?;
                                let v: LitStr = v.parse()?;
                                id = v.value();
                            }
                            Ok(())
                        })
                        .ok()
                    });

                let ty = &f.ty;

                quote! {(#id.into(), <#ty as luals_gen::ToLuaLsType>::lua_ls_type())}
            });

            quote! {
                luals_gen::LuaLsType::TableLiteral(vec![#(#items),*])
            }
        }
        syn::Data::Enum(_) => panic!("Only structs are supported"),
        syn::Data::Union(_) => panic!("Only structs are supported"),
    };

    let id = &input.ident;
    let expanded = quote! {
        impl luals_gen::ToLuaLsType for #id {
            fn lua_ls_type() -> luals_gen::LuaLsType {
                #expanded
            }
        }
    };

    // Build the output, possibly using quasi-quotation
    // panic!("{}", expanded.to_string());
    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}

const CASES: [(&'static str, convert_case::Case); 8] = [
    ("lowercase", convert_case::Case::Lower),
    ("UPPERCASE", convert_case::Case::Upper),
    ("PascalCase", convert_case::Case::Pascal),
    ("camelCase", convert_case::Case::Camel),
    ("snake_case", convert_case::Case::Snake),
    ("SCREAMING_SNAKE_CASE", convert_case::Case::ScreamingSnake),
    ("kebab-case", convert_case::Case::Kebab),
    ("SCREAMING-KEBAB-CASE", convert_case::Case::Kebab),
];
