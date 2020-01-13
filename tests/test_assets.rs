use std::io::Read;

#[test]
fn parse_assets_syntax_only() {
    let reader = xz2::read::XzDecoder::new(test_data::ASSETS_TAR);
    let mut archive = tar::Archive::new(reader);

    crossbeam::scope(|scope| {
        for file in archive.entries().unwrap() {
            let mut file = file.unwrap();

            if file.header().entry_type() != tar::EntryType::Regular
                || file.path().unwrap().extension() != Some("ninja".as_ref())
            {
                continue;
            }

            let mut data = Vec::new();
            file.read_to_end(&mut data).unwrap();

            scope.spawn(move || {
                let statements = ninja_build_syntax::parse(&data);

                for statement in statements {
                    statement.unwrap();
                }
            });
        }
    })
}
