#!/usr/bin/perl
use strict; use warnings;

use Test::More;
use Dean::TestUtil qw/:html/;


# uri
{
    my @uri =
    ( "http://dean.serenevy.net/code/projects/Dean::Util/Dean::Util.html#_html___html_utilities" =>
      ["http://dean.serenevy.net/code/projects/", "Dean::Util", "Dean::Util.html#_html___html_utilities" ],

      "http://serenevy.net/test/baz?foo=bar&baz=bip&greet=Hello%20World%21#23" =>
      ["http://serenevy.net/?foo=bar", "test/#23", "/baz/?baz=bip", { greet => "Hello World!" } ],

      "http://serenevy.net/test/baz?foo=barf&baz=bip&greet=Hello%20World%21#23" =>
      ["http://serenevy.net/", "test/?foo=barf&baz=bip#23", { greet => "Hello World!" }, "/baz/" ],

      "/test/baz" =>
      ["/test", "baz" ],

      "test/baz" =>
      ["test", "baz" ],

  );

    while (my ($uri, $args) = splice @uri, 0, 2) {
        is( uri(@$args), $uri, "uri: $uri" );
    }
}

# xml_attr
{
    my %tests = (
        'class="foo" bar' => [ class => "foo", \"bar" => 1 ],
        'class="foo"'     => [ \'class="foo"' => 1, \'bar="baz"' => 0 ],
        'foo="ba&quot;r"' => [ foo => 'ba"r' ],
    );

    while (my ($res, $arg) = each %tests) {
        is xml_attr(@$arg), $res,  "xml_attr: $res";
    }
}

# xml_tag
{
    my %tests = (
        '<iframe width="640" height="480" frameborder="0" src="//www.youtube-nocookie.com/embed/12345?rel=0" allowfullscreen></iframe>' =>
        [ iframe => "", (
            width => 640, height => 480, frameborder => 0,
            src   => "//www.youtube-nocookie.com/embed/12345?rel=0",
            \"allowfullscreen", 1,
        )],

        '<iframe width="640" height="480" frameborder="0" src="//www.youtube-nocookie.com/embed/12345?rel=0"></iframe>' =>
        [ iframe => "", (
            width => 640, height => 480, frameborder => 0,
            src   => "//www.youtube-nocookie.com/embed/12345?rel=0",
            \"allowfullscreen", 0,
        )],
    );

    while (my ($res, $arg) = each %tests) {
        is xml_tag(@$arg), $res,  "xml_tag: $res";
    }
}



done_testing;
