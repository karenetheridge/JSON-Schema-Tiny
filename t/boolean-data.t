use strict;
use warnings;
use 5.016;
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
    \2,
  );

  cmp_deeply(
    evaluate(
      $_,
      $test_schema = {
        enum => [ false, true ],
        allOf => [ { type => 'boolean' }, { type => ['boolean','object'] } ],
        anyOf => [ { const => false }, { const => true } ],
      }
    ),
    $failure_result = {
      valid => false,
      errors => [
        {
          instanceLocation => '',
          keywordLocation => '/enum',
          error => 'value does not match',
        },
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
    \2,
  );
};

subtest '$MOJO_BOOLEANS = 1' => sub {
  local $JSON::Schema::Tiny::MOJO_BOOLEANS = 1;
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
    \2,
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
        \2,
      ],
      { uniqueItems => true },
    ),
    { valid => true },
    'items are still all considered unique even though some are treated identically',
  );
};

done_testing;
