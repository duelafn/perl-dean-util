use Module::Build;

my $build = Module::Build->new(
    module_name => 'Dean::Util',
    license  => 'CC0_1_0',
    configure_requires => {
        'Module::Build'  => 0,
    },
    requires => {
        "perl" => '5.20.0'
    },
    recommends => {
        "Color::Calc"    => 0,
        "Date::Manip"    => 0,
        "DateTime"       => 0,
        "LWP::Simple"    => 0,
        "Path::Class"    => 0,
        "PerlIO::gzip"   => 0,
        "RTF::Tokenizer" => 0,
        "Term::ReadKey"  => 0,
        "Text::Balanced" => 0,
        "URI"            => 0,
        "URI::Escape"    => 0,
        "XML::LibXML"    => 0,
        "YAML"           => 0,
    },
    test_requires => {
        "Color::Calc"    => 0,
        "Date::Manip"    => 0,
        "DateTime"       => 0,
        "LWP::Simple"    => 0,
        "Path::Class"    => 0,
        "PerlIO::gzip"   => 0,
        "Term::ReadKey"  => 0,
        "Text::Balanced" => 0,
        "URI"            => 0,
        "URI::Escape"    => 0,
        "XML::LibXML"    => 0,
        "YAML"           => 0,
    },
);
$build->create_build_script;
