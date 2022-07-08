#[derive(PartialEq, Eq, Copy, Clone, Hash, Debug)]
enum Handsign {
    Rock,
    Scissors,
    Paper
}

use Handsign::*;

impl Handsign {
    fn prey(&self) -> Handsign {
        match self {
            Rock => Scissors,
            Scissors => Paper,
            Paper => Rock,
        }
    }
}

use std::collections::HashSet;

fn winner(hs: &[Handsign]) -> Option<Handsign> {
    let mut set = HashSet::new();
    for h in hs.into_iter() {
        set.insert(h);
    }
    if set.len() == 2 {
        let mut i = set.into_iter();
        let a = *i.next().unwrap();
        let b = *i.next().unwrap();
        Some(if a.prey() == b { a } else { b })
    } else {
        None
    }
}

fn main() {
    println!("{:?}", winner(&[Scissors, Rock]));
    println!("{:?}", winner(&[Rock, Scissors]));
    println!("{:?}", winner(&[Rock, Rock]));
    println!("{:?}", winner(&[Rock, Scissors, Paper]));
    println!("{:?}", winner(&[Rock, Scissors, Rock, Scissors]));
}

#[test]
fn decided() {
    assert_eq!(winner(&[Scissors, Rock]), Some(Rock));
}

#[test]
fn undecided_one_kind() {
    assert_eq!(winner(&[Rock, Rock]), None);
}

#[test]
fn undecided_three_kind() {
    assert_eq!(winner(&[Rock, Scissors, Paper]), None);
}

#[test]
fn allow_repeatition() {
    assert_eq!(winner(&[Rock, Scissors, Rock]), Some(Rock));
}
