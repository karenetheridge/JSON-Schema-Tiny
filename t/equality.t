# vim: set ft=perl ts=8 sts=2 sw=2 tw=100 et :
use strictures 2;
use 5.020;
use stable 0.031 'postderef';
use experimental 'signatures';
no autovivification warn => qw(fetch store exists delete);
use if "$]" >= 5.022, experimental => 're_strict';
no if "$]" >= 5.031009, feature => 'indirect';
no if "$]" >= 5.033001, feature => 'multidimensional';
no if "$]" >= 5.033006, feature => 'bareword_filehandles';
use utf8;
use open ':std', ':encoding(UTF-8)'; # force stdin, stdout, stderr into utf8

use Test::More 0.96;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use JSON::Schema::Tiny ();
use lib 't/lib';
use Helper;

sub is_type { goto &JSON::Schema::Tiny::is_type }
sub get_type { goto &JSON::Schema::Tiny::get_type }
sub is_equal { goto &JSON::Schema::Tiny::is_equal }

subtest 'equality, using inflated data' => sub {
  foreach my $test (
    [ undef, undef, true ],
    [ undef, false, false, '', 'wrong type: null vs boolean' ],
    [ undef, true , false, '', 'wrong type: null vs boolean' ],
    [ undef, 1, false, '', 'wrong type: null vs integer' ],
    [ undef, '1', false, '', 'wrong type: null vs string' ],
    [ [qw(a b c)], [qw(a b c)], true ],
    [ [qw(a b c)], [qw(a b)], false, '', 'element count differs: 3 vs 2' ],
    [ [qw(a b)], [qw(b a)], false, '/0', 'strings not equal' ],
    [ 1, 1, true ],
    [ 1, 1.0, true ],
    [ 1, '1.0', false, '', 'wrong type: integer vs string' ],
    [ '1.1', 1.1, false, '', 'wrong type: string vs number' ],
    [ '1', 1, false, '', 'wrong type: string vs integer' ],
    [ '1.1', 1.1, false, '', 'wrong type: string vs number' ],
    [ [1,2], [2,1], false, '/0', 'integers not equal' ],
    [ { a => 1, b => 2 }, { b => 2, a => 1 }, true ],
    [ { a => 1 }, { a => 1.0 }, true ],
    [ [qw(école ಠ_ಠ)], ["\x{e9}cole", "\x{0ca0}_\x{0ca0}"], true ],
    [ { a => 1, b => 2 }, { a => 1, b => 3 }, false, '/b', 'integers not equal' ],
    [ { a => { b => 1, c => 2 }, d => { e => 3, f => 4 } },
      { a => { b => 1, c => 2 }, d => { e => 3, f => 5 } }, false, '/d/f', 'integers not equal' ],
    [ [ { a => 1 } ], [ { a => 1, b => 2 } ], false, '/0', 'property count differs: 1 vs 2' ],
    [ [ { a => 1 } ], [ { b => 2 } ], false, '/0', 'property names differ starting at position 0 ("a" vs "b")' ],
    [ { foo => [ [ 0 ] ] }, { foo => [ [ 0, 1 ] ] }, false, '/foo/0', 'element count differs: 1 vs 2' ],
  ) {
    my ($x, $y, $expected, $diff_path, $error) = @$test;
    my @types = map get_type($_), $x, $y;
    my $result = is_equal($x, $y, my $state = {});

    ok(!($result xor $expected), json_sprintf('%s == %s is %s', $x, $y, $expected));
    is($state->{path}, $diff_path // '', 'two instances differ at the expected place') if not $expected;
    is($state->{error}, $error // '', 'error is correct') if not $expected;
    is($state->{error}, undef, 'error is undefined') if $expected;
    isnt($state->{error}, 'uh oh', 'no unexpected error encountered');

    ok(is_type($types[0], $x), 'type of arg 0 was not mutated while making equality check');
    ok(is_type($types[1], $y), 'type of arg 1 was not mutated while making equality check');

    note '';
  }
};

my $decoder = (Mojo::JSON::JSON_XS ? 'Cpanel::JSON::XS' : 'JSON::PP')->new->allow_nonref(1)->utf8(0);

subtest 'equality, using JSON strings' => sub {
  foreach my $test (
    [ 'null', 'null', true ],
    [ 'null', 1, false ],
    [ '["a","b","c"]', '["a","b","c"]', true ],
    [ '["a","b","c"]', '["a","b"]', false ],
    [ '["a","b"]', '["b","a"]', false, '/0' ],
    [ '1', '1', true ],
    [ '1', '1.0', true ],
    [ '10', '1e1', true ],
    [ '[1,2]', '[2,1]', false, '/0' ],
    [ '{"a":1,"b":2}', '{"a":1,"b":2}', true ],
    [ '{"a":1}', '{"a":1.0}', true ],
    [ '["école","ಠ_ಠ"]', qq{["\x{e9}cole", "\x{0ca0}_\x{0ca0}"]}, true ],
    [ '{"a":1,"b":2}', '{"b":3,"a":1}', false, '/b' ],
    [ '{"a":{"b":1,"c":2},"d":{"e":3,"f":4}}',
      '{"a":{"b":1,"c":2},"d":{"e":3,"f":5}}', false, '/d/f' ],
  ) {
    my ($x, $y, $expected, $diff_path) = @$test;
    ($x, $y) = map $decoder->decode($_), $x, $y;

    my @types = map get_type($_), $x, $y;
    my $result = is_equal($x, $y, my $state = {});
    ok(!($result xor $expected), json_sprintf('%s == %s is %s', $x, $y, $expected));
    is($state->{path}, $diff_path // '', 'two instances differ at the expected place') if not $expected;
    isnt($state->{error}, 'uh oh', 'no unexpected error encountered');

    ok(is_type($types[0], $x), 'type of arg 0 was not mutated while making equality check');
    ok(is_type($types[1], $y), 'type of arg 1 was not mutated while making equality check');

    note '';
  }
};

subtest 'equality, using scalarref booleans' => sub {
  local $JSON::Schema::Tiny::SCALARREF_BOOLEANS = 1;

  foreach my $test (
    [ \0, true, false ],
    [ \1, true, true ],
    [ \0, false, true ],
    [ \1, false, false ],
    [ undef, \0, false ],
    [ undef, false, false ],
  ) {
    my ($x, $y, $expected, $diff_path) = @$test;
    my @types = map get_type($_), $x, $y;
    my $result = is_equal($x, $y, my $state = {});

    ok(!($result xor $expected), json_sprintf('%s == %s is %s', $x, $y, $expected));
    is($state->{path}, $diff_path // '', 'two instances differ at the expected place') if not $expected;
    isnt($state->{error}, 'uh oh', 'no unexpected error encountered');

    ok(is_type($types[0], $x), 'type of arg 0 was not mutated while making equality check');
    ok(is_type($types[1], $y), 'type of arg 1 was not mutated while making equality check');

    note '';
  }
};

subtest 'equality, using stringy numbers' => sub {
  local $JSON::Schema::Tiny::STRINGY_NUMBERS = 1;

  foreach my $test (
    [ 1, 1, true ],
    [ 1, 1.0, true ],
    [ 1, '1.0', true ],
    [ '1.1', 1.1, true ],
    [ '1', 1, true ],
    [ '1.1', 1.1, true ],
    [ '1', '1.00', true ],
    [ '1.10', '1.1000', true ],
    [ 'x', 'x', true ],
    [ 'x', 'y', false ],
  ) {
    my ($x, $y, $expected, $diff_path) = @$test;
    my @types = map get_type($_), $x, $y;
    my $result = is_equal($x, $y, my $state = { stringy_numbers => 1 });

    ok(!($result xor $expected), json_sprintf('%s == %s is %s', $x, $y, $expected));
    is($state->{path}, $diff_path // '', 'two instances differ at the expected place') if not $expected;
    isnt($state->{error}, 'uh oh', 'no unexpected error encountered');

    ok(is_type($types[0], $x), 'type of arg 0 was not mutated while making equality check');
    ok(is_type($types[1], $y), 'type of arg 1 was not mutated while making equality check');

    note '';
  }
};

done_testing;
