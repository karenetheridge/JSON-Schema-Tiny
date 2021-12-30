use strict;
use warnings;
use 5.020;
use experimental qw(signatures postderef);
no if "$]" >= 5.031009, feature => 'indirect';
no if "$]" >= 5.033001, feature => 'multidimensional';
no if "$]" >= 5.033006, feature => 'bareword_filehandles';
use open ':std', ':encoding(UTF-8)'; # force stdin, stdout, stderr into utf8

use Test::More 0.88;
use if $ENV{AUTHOR_TESTING}, 'Test::Warnings';
use Test::Fatal;
use Test::Deep;
use Data::Dumper;
use JSON::Schema::Tiny 'evaluate';
use lib 't/lib';
use Helper;

sub serialize { Data::Dumper->new([ $_[0] ])->Indent(0)->Terse(1)->Sortkeys(1)->Dump }

my ($test_schema, $failure_result);

subtest 'strict booleans (default)' => sub {
  cmp_deeply(
    evaluate($_, { type => 'boolean' }),
    { valid => true },
    'in data, '.serialize($_).' is a boolean',
  )
  foreach (
    false,
    true,
  );

  cmp_deeply(
    evaluate($_, { type => 'boolean' }),
    {
      valid => false,
      errors => [
        {
          instanceLocation => '',
          keywordLocation => '/type',
          error => 'wrong type (expected boolean)',
        },
      ],
    },
    'correct error generated from type for '.serialize($_),
  )
  foreach (
    undef,
    0,
    1,
    '0',
    '1',
    'false',
    'true',
    \0,
    \1,
  );

  cmp_deeply(
    evaluate(
      $_,
      $test_schema = {
        allOf => [ { type => 'boolean' }, { type => ['boolean','object'] } ],
        anyOf => [ { const => false }, { const => true } ],
        enum => [ false, true ],
      }
    ),
    $failure_result = {
      valid => false,
      errors => [
        {
          instanceLocation => '',
          keywordLocation => '/allOf/0/type',
          error => 'wrong type (expected boolean)',
        },
        {
          instanceLocation => '',
          keywordLocation => '/allOf/1/type',
          error => 'wrong type (expected one of boolean, object)',
        },
        {
          instanceLocation => '',
          keywordLocation => '/allOf',
          error => 'subschemas 0, 1 are not valid',
        },
        {
          instanceLocation => '',
          keywordLocation => '/anyOf/0/const',
          error => 'value does not match',
        },
        {
          instanceLocation => '',
          keywordLocation => '/anyOf/1/const',
          error => 'value does not match',
        },
        {
          instanceLocation => '',
          keywordLocation => '/anyOf',
          error => 'no subschemas are valid',
        },
        {
          instanceLocation => '',
          keywordLocation => '/enum',
          error => 'value does not match',
        },
      ],
    },
    'in data, '.serialize($_).' not is a boolean',
  )
  foreach (
    undef,
    0,
    1,
    '0',
    '1',
    'false',
    'true',
    \0,
    \1,
  );
};

subtest '$SCALARREF_BOOLEANS = 1' => sub {
  local $JSON::Schema::Tiny::SCALARREF_BOOLEANS = 1;
  cmp_deeply(
    evaluate($_, $test_schema),
    { valid => true },
    'in data, '.serialize($_).' is a boolean',
  )
  foreach (
    false,
    true,
    \0,
    \1,
  );

  cmp_deeply(
    evaluate($_, $test_schema),
    $failure_result,
    'correct error generated from type for '.serialize($_),
  )
  foreach (
    undef,
    0,
    1,
    '0',
    '1',
    'false',
    'true',
  );

  cmp_deeply(
    evaluate(
      [
        undef,
        0,
        1,
        '0',
        '1',
        'false',
        'true',
        \0,
        \1,
      ],
      { uniqueItems => true },
    ),
    { valid => true },
    'items are all considered unique when types differ, even when perl treats them similarly',
  );

  cmp_deeply(
    evaluate($_, { uniqueItems => true }),
    {
      valid => false,
      errors => [
        {
          instanceLocation => '',
          keywordLocation => '/uniqueItems',
          error => 'items at indices 0 and 1 are not unique',
        },
      ],
    },
    'scalarrefs compare as identical to their counterpart booleans',
  )
  foreach (
    [ \0, false ],
    [ false, \0 ],
    [ \1, true ],
    [ true, \1 ],
  );
};

done_testing;
