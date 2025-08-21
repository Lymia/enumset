use enumset::EnumRecord;
use enumset_derive::EnumSetType;
use std::collections::{BTreeMap, HashMap};

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
enum SmallEnum {
    A, B, C,
}

#[derive(EnumSetType, Debug)]
#[rustfmt::skip]
enum TestEnum {
    A, B, C, D, E, F, G, H,
}

macro_rules! do_tests {
    ($mod_name:ident, $repeat:ident, $from_fn:ident, $map:ident, $($deref:tt)*) => {
        mod $mod_name {
            use super::*;

            #[test]
            fn record_set_get() {
                let mut record = EnumRecord::<TestEnum, u32>::$repeat(0);
                assert_eq!(record[TestEnum::A], 0);
                assert_eq!(record[TestEnum::B], 0);
                assert_eq!(record[TestEnum::C], 0);
                assert_eq!(record[TestEnum::D], 0);
                record[TestEnum::A] = 100;
                record[TestEnum::B] = 200;
                record[TestEnum::C] = 300;
                record[TestEnum::D] = 400;
                assert_eq!(record[TestEnum::A], 100);
                assert_eq!(record[TestEnum::B], 200);
                assert_eq!(record[TestEnum::C], 300);
                assert_eq!(record[TestEnum::D], 400);
            }

            #[test]
            fn record_init_fn() {
                let record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32 * 100);
                assert_eq!(record[TestEnum::A], 0);
                assert_eq!(record[TestEnum::B], 100);
                assert_eq!(record[TestEnum::C], 200);
                assert_eq!(record[TestEnum::D], 300);
                assert_eq!(record[TestEnum::E], 400);
                assert_eq!(record[TestEnum::F], 500);
                assert_eq!(record[TestEnum::G], 600);
                assert_eq!(record[TestEnum::H], 700);
            }

            #[test]
            fn record_map() {
                let record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32 * 100);
                let record = record.$map(|_, v| v.to_string());
                assert_eq!(record[TestEnum::A].as_str(), "0");
                assert_eq!(record[TestEnum::B].as_str(), "100");
                assert_eq!(record[TestEnum::C].as_str(), "200");
                assert_eq!(record[TestEnum::D].as_str(), "300");
                assert_eq!(record[TestEnum::E].as_str(), "400");
                assert_eq!(record[TestEnum::F].as_str(), "500");
                assert_eq!(record[TestEnum::G].as_str(), "600");
                assert_eq!(record[TestEnum::H].as_str(), "700");
            }

            #[test]
            fn record_iter() {
                let record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32 * 100);
                let mut iter = record.into_iter();
                assert_eq!(iter.next(), Some((TestEnum::A, 0)));
                assert_eq!(iter.next(), Some((TestEnum::B, 100)));
                assert_eq!(iter.next(), Some((TestEnum::C, 200)));
                assert_eq!(iter.next(), Some((TestEnum::D, 300)));
                assert_eq!(iter.next(), Some((TestEnum::E, 400)));
                assert_eq!(iter.next(), Some((TestEnum::F, 500)));
                assert_eq!(iter.next(), Some((TestEnum::G, 600)));
                assert_eq!(iter.next(), Some((TestEnum::H, 700)));
                assert_eq!(iter.next(), None);
                assert_eq!(iter.next(), None);
            }

            #[test]
            fn record_to_vec() {
                let record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32 * 100);
                let vec: Vec<_> = record.into_iter().collect();
                assert_eq!(vec, vec![
                    (TestEnum::A, 0),
                    (TestEnum::B, 100),
                    (TestEnum::C, 200),
                    (TestEnum::D, 300),
                    (TestEnum::E, 400),
                    (TestEnum::F, 500),
                    (TestEnum::G, 600),
                    (TestEnum::H, 700),
                ]);
            }

            #[test]
            fn record_iter_mut() {
                let mut record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32);
                for (_, x) in &mut $($deref)* record {
                    *x *= 100;
                }
                let mut iter = record.into_iter();
                assert_eq!(iter.next(), Some((TestEnum::A, 0)));
                assert_eq!(iter.next(), Some((TestEnum::B, 100)));
                assert_eq!(iter.next(), Some((TestEnum::C, 200)));
                assert_eq!(iter.next(), Some((TestEnum::D, 300)));
                assert_eq!(iter.next(), Some((TestEnum::E, 400)));
                assert_eq!(iter.next(), Some((TestEnum::F, 500)));
                assert_eq!(iter.next(), Some((TestEnum::G, 600)));
                assert_eq!(iter.next(), Some((TestEnum::H, 700)));
                assert_eq!(iter.next(), None);
                assert_eq!(iter.next(), None);
            }

            #[test]
            fn record_iter_both_ends() {
                let record = EnumRecord::<TestEnum, u32>::$from_fn(|x| x as u32 * 100);
                assert_eq!(record.iter().count(), 8);
                let mut iter = record.iter();
                assert_eq!(iter.next(), Some((TestEnum::A, &0)));
                assert_eq!(iter.next_back(), Some((TestEnum::H, &700)));
                assert_eq!(iter.next_back(), Some((TestEnum::G, &600)));
                assert_eq!(iter.next(), Some((TestEnum::B, &100)));
                assert_eq!(iter.next(), Some((TestEnum::C, &200)));
                assert_eq!(iter.next(), Some((TestEnum::D, &300)));
                assert_eq!(iter.next(), Some((TestEnum::E, &400)));
                assert_eq!(iter.next_back(), Some((TestEnum::F, &500)));
                assert_eq!(iter.next(), None);
                assert_eq!(iter.next_back(), None);
                assert_eq!(iter.next(), None);
            }

            #[test]
            fn works_in_ops() {
                let rec_a = EnumRecord::<SmallEnum, u32>::$from_fn(|x| x as u32 * 100);
                let rec_b = EnumRecord::<SmallEnum, u32>::$from_fn(|x| x as u32 * 200);
                let rec_c = EnumRecord::<SmallEnum, u32>::$from_fn(|x| x as u32 * 300);
                let rec_d = EnumRecord::<SmallEnum, u32>::$from_fn(|x| x as u32 * 400);

                let mut map = HashMap::new();
                map.insert(rec_a.clone(), 3);
                map.insert(rec_b.clone(), 7);
                assert_eq!(map.get(&rec_a), Some(&3));
                assert_eq!(map.get(&rec_b), Some(&7));
                assert_eq!(map.get(&rec_c), None);
                assert_eq!(map.get(&rec_d), None);
                map.insert(rec_a.clone(), 5);
                assert_eq!(map.get(&rec_a), Some(&5));

                let mut map = BTreeMap::new();
                map.insert(rec_a.clone(), 3);
                map.insert(rec_b.clone(), 7);
                assert_eq!(map.get(&rec_a), Some(&3));
                assert_eq!(map.get(&rec_b), Some(&7));
                assert_eq!(map.get(&rec_c), None);
                assert_eq!(map.get(&rec_d), None);
                map.insert(rec_a.clone(), 5);
                assert_eq!(map.get(&rec_a), Some(&5));
            }

            #[test]
            fn default_impl() {
                let rec_a = EnumRecord::<SmallEnum, u32>::$from_fn(|_| 0);
                let rec_b = EnumRecord::<SmallEnum, u32>::default();
                assert_eq!($($deref)* rec_a, rec_b);
            }
        }
    };
}

do_tests!(unboxed, repeat, from_fn, map /* */,);

#[cfg(feature = "alloc")]
do_tests!(boxed, repeat_boxed, from_fn_boxed, map_boxed, *);

#[test]
fn debug_impl() {
    let rec_a = EnumRecord::<SmallEnum, u32>::from_fn(|x| x as u32 * 100);
    let rec_b = EnumRecord::<SmallEnum, u32>::from_fn(|x| x as u32 * 200);
    let rec_c = EnumRecord::<SmallEnum, u32>::from_fn(|x| x as u32 * 300);
    let rec_d = EnumRecord::<SmallEnum, u32>::from_fn(|x| x as u32 * 400);
    assert_eq!(format!("{rec_a:?}"), "{A: 0, B: 100, C: 200}");
    assert_eq!(format!("{rec_b:?}"), "{A: 0, B: 200, C: 400}");
    assert_eq!(format!("{rec_c:?}"), "{A: 0, B: 300, C: 600}");
    assert_eq!(format!("{rec_d:?}"), "{A: 0, B: 400, C: 800}");
}
