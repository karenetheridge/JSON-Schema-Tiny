# vim: set ft=perl ts=8 sts=2 sw=2 tw=100 et :
use strictures 2;
use 5.020;
use experimental qw(signatures postderef);
no if "$]" >= 5.031009, feature => 'indirect';
no if "$]" >= 5.033001, feature => 'multidimensional';
no if "$]" >= 5.033006, feature => 'bareword_filehandles';
use open ':std', ':encoding(UTF-8)'; # force stdin, stdout, stderr into utf8

use Test::More;
use List::Util 1.50 'head';
use Config;
use lib 't/lib';
use Acceptance;

BEGIN {
  my @variables = qw(AUTHOR_TESTING AUTOMATED_TESTING EXTENDED_TESTING);

  plan skip_all => 'These tests may fail if the test suite continues to evolve! They should only be run with '
      .join(', ', map $_.'=1', head(-1, @variables)).' or '.$variables[-1].'=1'
    if not -d '.git' and not grep $ENV{$_}, @variables;
}

my $version = 'draft2019-09';

my $orig_warn_handler = $SIG{__WARN__};
$SIG{__WARN__} = sub {
  return if $_[0] =~ /^no-longer-supported "dependencies" keyword present \(at location ""\): this should be rewritten as "dependentSchemas" or "dependentRequired"/;
  goto &$orig_warn_handler if $orig_warn_handler;
};

acceptance_tests(
  acceptance => {
    specification => $version,
    skip_dir => 'optional/format',
  },
  evaluator => {
    specification_version => $version,
  },
  output_file => $version.'-acceptance.txt',
  test => {
    $ENV{NO_TODO} ? () : ( todo_tests => [
      # unsupported keywords or subfeatures
      { file => [ qw(
          anchor.json
          id.json
          recursiveRef.json
          refRemote.json
          unevaluatedItems.json
          unevaluatedProperties.json
          vocabulary.json
        ) ] },
      { file => 'defs.json', group_description => [ 'valid definition', 'validate definition against metaschema' ] },
      { file => 'ref.json', group_description => [
          'remote ref, containing refs itself',
          'Recursive references between schemas',
          'refs with relative uris and defs',
          'relative refs with absolute uris and defs',
          '$id must be resolved against nearest parent, not just immediate parent',
          'order of evaluation: $id and $ref',
          'order of evaluation: $id and $anchor and $ref',
        ] },
      { file => 'unknownKeyword.json', group_description => '$id inside an unknown keyword is not a real identifier', test_description => 'type matches second anyOf, which has a real schema in it' },
      # I am not interested in back-supporting "dependencies"
      { file => 'optional/dependencies-compatibility.json' },
      # various edge cases that are difficult to accomodate
      { file => 'optional/ecmascript-regex.json', group_description => '\w in patterns matches [A-Za-z0-9_], not unicode letters', test_description => [ 'literal unicode character in json string', 'unicode character in hex format in string' ] },
      { file => 'optional/ecmascript-regex.json', group_description => '\d in pattern matches [0-9], not unicode digits', test_description => 'non-ascii digits (BENGALI DIGIT FOUR, BENGALI DIGIT TWO)' },
      { file => 'optional/ecmascript-regex.json', group_description => '\w in patternProperties matches [A-Za-z0-9_], not unicode letters', test_description => [ 'literal unicode character in json string', 'unicode character in hex format in string' ] },
      { file => 'optional/ecmascript-regex.json', group_description => '\d in patternProperties matches [0-9], not unicode digits', test_description => 'non-ascii digits (BENGALI DIGIT FOUR, BENGALI DIGIT TWO)' },
      { file => 'optional/ecmascript-regex.json', group_description => [ 'ECMA 262 \d matches ascii digits only', 'ECMA 262 \D matches everything but ascii digits', 'ECMA 262 \w matches ascii letters only', 'ECMA 262 \W matches everything but ascii letters' ] }, # TODO, see test suite PR#505
      { file => 'optional/ecmascript-regex.json', group_description => 'ECMA 262 \s matches whitespace', test_description => 'zero-width whitespace matches' },
      { file => 'optional/ecmascript-regex.json', group_description => 'ECMA 262 \S matches everything but whitespace', test_description => 'zero-width whitespace does not match' },
      "$]" < 5.017001 ? ( # in Unicode 6.2, vertical tab was added to \s
        { file => 'optional/ecmascript-regex.json', group_description => 'ECMA 262 \s matches whitespace', test_description => 'Line tabulation matches' },
        { file => 'optional/ecmascript-regex.json', group_description => 'ECMA 262 \S matches everything but whitespace', test_description => 'Line tabulation does not match' },
      ) : (),
    ] ),
  },
);

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
see t/results/draft2019-09-acceptance.txt for test results
