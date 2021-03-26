# vim: set ft=perl ts=8 sts=2 sw=2 tw=100 et :
use strict;
use warnings;
use 5.016;
no if "$]" >= 5.031009, feature => 'indirect';
no if "$]" >= 5.033001, feature => 'multidimensional';
no if "$]" >= 5.033006, feature => 'bareword_filehandles';
use open ':std', ':encoding(UTF-8)'; # force stdin, stdout, stderr into utf8

use Test::More;
use List::Util 1.50 'head';
use Config;

BEGIN {
  my @variables = qw(AUTHOR_TESTING AUTOMATED_TESTING EXTENDED_TESTING);

  plan skip_all => 'These tests may fail if the test suite continues to evolve! They should only be run with '
      .join(', ', map $_.'=1', head(-1, @variables)).' or '.$variables[-1].'=1'
    if not -d '.git' and not grep $ENV{$_}, @variables;
}

use if $ENV{AUTHOR_TESTING}, 'Test::Warnings' => ':fail_on_warning';
use Test::JSON::Schema::Acceptance 1.004;
use JSON::Schema::Tiny 'evaluate';

foreach my $env (qw(AUTHOR_TESTING AUTOMATED_TESTING EXTENDED_TESTING NO_TODO TEST_DIR NO_SHORT_CIRCUIT)) {
  note $env.': '.($ENV{$env} // '');
}
note '';

# TODO: test draft7 and draft202012 as well!

my $accepter = Test::JSON::Schema::Acceptance->new(
  $ENV{TEST_DIR} ? (test_dir => $ENV{TEST_DIR}) : (specification => 'draft2019-09'),
  include_optional => 1,
  skip_dir => 'optional/format',
  verbose => 1,
);

my $encoder = JSON::MaybeXS->new(allow_nonref => 1, utf8 => 0, convert_blessed => 1, canonical => 1, pretty => 1);
$encoder->indent_length(2) if $encoder->can('indent_length');

$accepter->acceptance(
  validate_data => sub {
    my ($schema, $instance_data) = @_;
    my $result = evaluate($instance_data, $schema);
    my $result_short = $ENV{NO_SHORT_CIRCUIT} || do {
      local $JSON::Schema::Tiny::SHORT_CIRCUIT = 1;
      evaluate($instance_data, $schema);
    };

    note 'result: ', $encoder->encode($result);

    note 'short-circuited result: ', ($encoder->encode($result_short) ? 'true' : 'false')
      if not $ENV{NO_SHORT_CIRCUIT} and ($result->{valid} xor $result_short->{valid});

    die 'results inconsistent between short_circuit = false and true'
      if not $ENV{NO_SHORT_CIRCUIT} and ($result->{valid} xor $result_short->{valid});


    # if any errors contain an exception, generate a warning so we can be sure
    # to count that as a failure (an exception would be caught and perhaps TODO'd).
    # (This might change if tests are added that are expected to produce exceptions.)
    foreach my $r ($result, ($ENV{NO_SHORT_CIRCUIT} ? () : $result_short)) {
      map warn('evaluation generated an exception: '.$encoder->encode($_)),
        grep +($_->{error} =~ /^EXCEPTION/
            && $_->{error} !~ /but short_circuit is enabled/            # unevaluated*
            && $_->{error} !~ /(max|min)imum value is not a number$/),  # optional/bignum.json
          @{$r->{errors}};
    }

    $result->{valid};
  },
  @ARGV ? (tests => { file => \@ARGV }) : (),
  # XXX skip_tests ...
  $ENV{NO_TODO} ? () : ( todo_tests => [
    { file => [
        'optional/bignum.json',                     # TODO: see JSD2 issue #10
        'optional/content.json',                    # removed in TJSA 1.003
        'optional/ecmascript-regex.json',           # TODO: see JSD2 issue #27
        'optional/float-overflow.json',             # see slack logs re multipleOf algo
      ] },
    # various edge cases that are difficult to accomodate
    $Config{ivsize} < 8 || $Config{nvsize} < 8 ?    # see JSD2 issue #10
      { file => 'const.json',
        group_description => 'float and integers are equal up to 64-bit representation limits',
        test_description => 'float is valid' }
      : (),
    $Config{nvsize} >= 16 ? # see https://github.com/json-schema-org/JSON-Schema-Test-Suite/pull/438#issuecomment-714670854
      { file => 'multipleOf.json',
        group_description => 'invalid instance should not raise error when float division = inf',
        test_description => 'always invalid, but naive implementations may raise an overflow error' }
      : (),
  ] ),
);


# date        Test::JSON::Schema::Acceptance version
#                    result count of running *all* tests (with no TODOs)
# ----        -----  --------------------------------------
# 2020-05-02  0.991  Looks like you failed 272 tests of 739.
# 2020-05-05  0.991  Looks like you failed 211 tests of 739.
# 2020-05-05  0.992  Looks like you failed 225 tests of 775.
# 2020-05-06  0.992  Looks like you failed 193 tests of 775.
# 2020-05-06  0.992  Looks like you failed 190 tests of 775.
# 2020-05-06  0.992  Looks like you failed 181 tests of 775.
# 2020-05-07  0.992  Looks like you failed 177 tests of 775.
# 2020-05-07  0.992  Looks like you failed 163 tests of 775.
# 2020-05-07  0.992  Looks like you failed 161 tests of 775.
# 2020-05-07  0.992  Looks like you failed 150 tests of 775.
# 2020-05-08  0.993  Looks like you failed 150 tests of 776.
# 2020-05-08  0.993  Looks like you failed 117 tests of 776.
# 2020-05-08  0.993  Looks like you failed 107 tests of 776.
# 2020-05-08  0.993  Looks like you failed 116 tests of 776.
# 2020-05-08  0.993  Looks like you failed 110 tests of 776.
# 2020-05-08  0.993  Looks like you failed 97 tests of 776.
# 2020-05-11  0.993  Looks like you failed 126 tests of 776.
# 2020-05-11  0.993  Looks like you failed 98 tests of 776.
# 2020-05-12  0.994  Looks like you failed 171 tests of 959.
# 2020-05-13  0.995  Looks like you failed 171 tests of 959.
# 2020-05-14  0.996  Looks like you failed 171 tests of 992.
# 2020-05-19  0.997  Looks like you failed 171 tests of 994.
# 2020-05-22  0.997  Looks like you failed 163 tests of 994.
# 2020-06-01  0.997  Looks like you failed 159 tests of 994.
# 2020-06-08  0.999  Looks like you failed 176 tests of 1055.
# 2020-06-09  0.999  Looks like you failed 165 tests of 1055.
# 2020-06-10  0.999  Looks like you failed 104 tests of 1055.
# 2020-07-07  0.999  Looks like you failed 31 tests of 1055.
# 2020-08-13  1.000  Looks like you failed 44 tests of 1210.
# 2020-08-14  1.000  Looks like you failed 42 tests of 1210.
# 2020-10-16  1.001  Looks like you failed 42 tests of 1221.
# 2020-11-24  1.002  Looks like you failed 46 tests of 1233.
# 2020-12-04  1.003  Looks like you failed 40 tests of 1265.


END {
diag <<DIAG

###############################

Attention CPANTesters: you do not need to file a ticket when this test fails. I will receive the test reports and act on it soon. thank you!

###############################
DIAG
  if not Test::Builder->new->is_passing;
}

done_testing;
__END__

# Results using Test::JSON::Schema::Acceptance 1.003
# with commit 6505944d38c414039cd8f27e3312b9e3831a0a16 (2.0.0-299-g6505944)
# from git://github.com/json-schema-org/JSON-Schema-Test-Suite.git:
# specification version: draft2019-09
# optional tests included: yes
#
# filename                                    pass  todo-fail  fail
# -----------------------------------------------------------------
# additionalItems.json                          13          0     0
# additionalProperties.json                     15          0     0
# allOf.json                                    30          0     0
# anchor.json                                    6          0     0
# anyOf.json                                    18          0     0
# boolean_schema.json                           18          0     0
# const.json                                    50          0     0
# contains.json                                 18          0     0
# content.json                                  18          0     0
# default.json                                   4          0     0
# defs.json                                      2          0     0
# dependentRequired.json                        20          0     0
# dependentSchemas.json                         13          0     0
# enum.json                                     33          0     0
# exclusiveMaximum.json                          4          0     0
# exclusiveMinimum.json                          4          0     0
# format.json                                  114          0     0
# id.json                                       13          0     0
# if-then-else.json                             26          0     0
# infinite-loop-detection.json                   2          0     0
# items.json                                    26          0     0
# maxContains.json                              10          0     0
# maxItems.json                                  4          0     0
# maxLength.json                                 5          0     0
# maxProperties.json                             8          0     0
# maximum.json                                   8          0     0
# minContains.json                              23          0     0
# minItems.json                                  4          0     0
# minLength.json                                 5          0     0
# minProperties.json                             6          0     0
# minimum.json                                  11          0     0
# multipleOf.json                                9          0     0
# not.json                                      12          0     0
# oneOf.json                                    27          0     0
# pattern.json                                   9          0     0
# patternProperties.json                        22          0     0
# properties.json                               20          0     0
# propertyNames.json                            10          0     0
# recursiveRef.json                             32          0     0
# ref.json                                      34          0     0
# refRemote.json                                15          0     0
# required.json                                  9          0     0
# type.json                                     80          0     0
# unevaluatedItems.json                         33          0     0
# unevaluatedProperties.json                    51          0     0
# uniqueItems.json                              64          0     0
# optional/bignum.json                           2          7     0
# optional/ecmascript-regex.json                31          9     0
# optional/float-overflow.json                   0          1     0
# optional/non-bmp-regex.json                   12          0     0
# optional/refOfUnknownKeyword.json              4          0     0
# optional/format/date-time.json                10          1     0
# optional/format/date.json                      4          1     0
# optional/format/duration.json                 17          0     0
# optional/format/email.json                     9          0     0
# optional/format/hostname.json                 12          0     0
# optional/format/idn-email.json                 4          0     0
# optional/format/idn-hostname.json             29         16     0
# optional/format/ipv4.json                      6          0     0
# optional/format/ipv6.json                     29          0     0
# optional/format/iri-reference.json             5          2     0
# optional/format/iri.json                       8          1     0
# optional/format/json-pointer.json             32          0     0
# optional/format/regex.json                     2          0     0
# optional/format/relative-json-pointer.json     6          0     0
# optional/format/time.json                      3          0     0
# optional/format/uri-reference.json             7          0     0
# optional/format/uri-template.json              3          1     0
# optional/format/uri.json                      19          1     0
# optional/format/uuid.json                     12          0     0
# -----------------------------------------------------------------
# TOTAL                                       1224         40     0
