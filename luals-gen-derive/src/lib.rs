use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Attribute, DeriveInput, LitStr};

#[proc_macro_derive(ToLuaLsType)]
pub fn to_lua_ls_type(input: TokenStream) -> TokenStream {
    // Parse the input tokens into a syntax tree
    let input = parse_macro_input!(input as DeriveInput);
    let serde = input.attrs.iter().find(|x| x.path().is_ident("serde"));
    let id = &input.ident;
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
            let items = enumerate_fields(fields, rename_all);

            quote! {
                luals_gen::LuaLsTypeDef::TableLiteral(vec![#(#items),*])
            }
        }
        syn::Data::Enum(e) => {
            let tag_content = get_enum_attrs(serde.into_iter());
            let variants = e.variants.iter().map(|x| {
                let enum_id = id;
                let id = x.ident.to_string();
                let id_tokens = quote! {
                    luals_gen::LuaLsTypeDef::Named(
                    stringify!(#id).into()
                    )
                };
                let raw_type = if x.fields.is_empty() {
                    quote! {
                        luals_gen::LuaLsTypeDef::TableLiteral(vec![])
                    }
                } else {
                    let items = enumerate_fields(&x.fields, rename_all);
                    quote! {
                        luals_gen::LuaLsTypeDef::TableLiteral(vec![#(#items),*])
                    }
                };

                let variant = match &tag_content {
                    Some((None, None)) => {
                        if x.fields.is_empty() {
                            raw_type
                        } else {
                            let key_id = format_ident!("{}_key", id);
                            let value_id = format_ident!("{}_value", id);

                            quote! {
                                luals_gen::LuaLsTypeDef::Table {
                                    key: luals_gen::LuaLsType::Named(stringify!(#key_id).into(), #id_tokens),
                                    value: luals_gen::LuaLsType::Named(stringify!(#value_id).into(), #raw_type)
                                }
                            }
                        }
                    }
                    Some((None, Some(_))) => panic!("Invalid configuration"),
                    Some((Some(tag), None)) => {
                        quote! {
                            #raw_type.with_tag(#tag, stringify!(#id))
                        }
                    }
                    Some((Some(tag), Some(content))) => {
                        let tag_id = format_ident!("{}_tag", id);
                        let content_id = format_ident!("{}_content", id);
                        quote! {
                            luals_gen::LuaLsTypeDef::TableLiteral(
                                vec![
                                    ((#tag).into(),luals_gen::LuaLsType::Named(stringify!(#tag_id).into(), #id_tokens)),
                                    ((#content).into(), luals_gen::LuaLsType::Named(stringify!(#content_id).into(), #raw_type))
                                ])
                        }
                    }
                    None => raw_type,
                };

                let variant_id = format_ident!("{}_variant_{}", enum_id, id);


                let variant = quote! {
                    luals_gen::LuaLsType::Named(stringify!(#variant_id).into(), #variant)
                };
                variant
            });

            quote! {
                luals_gen::LuaLsTypeDef::Union(vec![#(#variants),*]
                )
            }
        }
        syn::Data::Union(_) => panic!("Unions are not supported"),
    };

    let expanded = quote! {
        impl luals_gen::ToLuaLsType for #id {
            fn lua_ls_type() -> luals_gen::LuaLsType {
                luals_gen::LuaLsType::Named(stringify!(#id).into(),
                #expanded)
            }
        }
    };

    // Build the output, possibly using quasi-quotation
    // panic!("{}", expanded.to_string());
    // Hand the output tokens back to the compiler
    TokenStream::from(expanded)
}

fn enumerate_fields(
    fields: &syn::Fields,
    rename_all: Option<convert_case::Case>,
) -> impl Iterator<Item = proc_macro2::TokenStream> + '_ {
    fields.iter().enumerate().map(move |(i, f)| {
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
    })
}

fn get_enum_attrs<'a>(
    attrs: impl Iterator<Item = &'a Attribute>,
) -> Option<(Option<String>, Option<String>)> {
    let mut content = None;
    let mut tag = None;
    let mut untagged = false;
    for ele in attrs {
        _ = ele.parse_nested_meta(|x| {
            if x.path.is_ident("tag") {
                let v = x.value()?;
                let v: LitStr = v.parse()?;
                tag = Some(v.value());
            } else if x.path.is_ident("content") {
                let v = x.value()?;
                let v: LitStr = v.parse()?;
                content = Some(v.value());
            } else if x.path.is_ident("untagged") {
                untagged = true;
            }
            Ok(())
        });
    }
    if untagged {
        None
    } else {
        Some((tag, content))
    }
}

const CASES: [(&str, convert_case::Case); 8] = [
    ("lowercase", convert_case::Case::Lower),
    ("UPPERCASE", convert_case::Case::Upper),
    ("PascalCase", convert_case::Case::Pascal),
    ("camelCase", convert_case::Case::Camel),
    ("snake_case", convert_case::Case::Snake),
    ("SCREAMING_SNAKE_CASE", convert_case::Case::ScreamingSnake),
    ("kebab-case", convert_case::Case::Kebab),
    ("SCREAMING-KEBAB-CASE", convert_case::Case::Kebab),
];
