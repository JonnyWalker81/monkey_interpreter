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
        ht.insert(String::from("true"), Token::True);
        ht.insert(String::from("false"), Token::False);
        ht.insert(String::from("if"), Token::If);
        ht.insert(String::from("else"), Token::Else);
        ht.insert(String::from("return"), Token::Return);
        ht.insert(String::from("while"), Token::While);
        ht.insert(String::from("for"), Token::For);

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
