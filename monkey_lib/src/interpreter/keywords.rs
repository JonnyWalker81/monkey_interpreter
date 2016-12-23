use interpreter::token::Token;
use std::collections::HashMap;

pub struct Keywords {
    keywords: HashMap<String, Token>
}

impl Keywords {
    pub fn new() -> Keywords {
        let mut ht = HashMap::new();
        ht.insert(String::from("fn"), Token::Function);
        ht.insert(String::from("let"), Token::Let);

        return Keywords {
            keywords: ht
        }
    }

    pub fn lookup_ident(&self, ident: &String) -> Token {
        match self.keywords.get(ident) {
            Some(token) => return token.clone(),
            None => return Token::Ident(ident.clone())
        };
    }
}
