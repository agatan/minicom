use std::sync::{Mutex, Arc};
use std::convert::From;
use std::ops::Deref;
use std::cmp::PartialEq;

#[derive(Debug)]
pub enum Typ {
    Int,
    Hole(Mutex<Option<ArcTyp>>),
}

#[derive(Debug, Clone)]
pub struct ArcTyp(Arc<Typ>);

impl ArcTyp {
    pub fn inner_most(&self) -> Option<ArcTyp> {
        match *self.0 {
            Typ::Hole(ref inner) => {
                let inner = inner.lock().unwrap();
                if let Some(ref typ) = *inner {
                    return Some(typ.clone())
                }
                None
            }
            _ => Some(self.clone()),
        }
    }

    pub fn hole() -> Self {
        ArcTyp(Arc::new(Typ::Hole(Mutex::default())))
    }
}

impl PartialEq for ArcTyp {
    fn eq(&self, other: &ArcTyp) -> bool {
        match (self.inner_most(), other.inner_most()) {
            (Some(t1), Some(t2)) => match (&*t1, &*t2) {
                (&Typ::Int, &Typ::Int) => true,
                _ => false,
            },
            _ => false,
        }
    }
}

impl From<Typ> for ArcTyp {
    fn from(typ: Typ) -> Self {
        ArcTyp(Arc::new(typ))
    }
}

impl Deref for ArcTyp {
    type Target = Typ;
    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
