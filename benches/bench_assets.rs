#[macro_use]
extern crate criterion;
extern crate tar;
extern crate xz2;

extern crate ninja_build_syntax as manifest_syntax;
extern crate test_data;

use std::convert::AsRef;
use std::io::Read;
use std::ops::Deref;
use std::path::Path;

use criterion::Criterion;

fn bench_parse_assets(c: &mut Criterion) {
    let reader = xz2::read::XzDecoder::new(test_data::ASSETS_TAR);
    let mut archive = tar::Archive::new(reader);
    let mut file = archive
        .entries()
        .unwrap()
        .find(|entry| {
            entry.as_ref().unwrap().path().unwrap().deref() == AsRef::<Path>::as_ref("assets/ninja/build.ninja")
        })
        .unwrap()
        .unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();

    c.bench_function("parse_assets", move |b| {
        b.iter(|| {
            let statements = manifest_syntax::parse(&data);

            for statement in statements {
                statement.unwrap();
            }
        })
    });
}

criterion_group!(benches, bench_parse_assets);
criterion_main!(benches);
