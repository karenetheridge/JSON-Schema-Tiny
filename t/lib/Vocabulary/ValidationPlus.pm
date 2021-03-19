package Vocabulary::ValidationPlus;

with 'JSON::Schema::Draft201909::Vocabulary';

sub vocabulary { 'https://github.com/karenetheridge/JSON-Schema-Draft201909/vocab/validation_plus' }

sub keywords {
  qw(between);
}

# XXX come up with a reasonable keyword that is easy to test.

sub _traverse_keyword_isSquare {
  my ($self, $schema, $state) = @_;

}

sub _eval_keyword_isSquare {
  my ($self, $data, $schema, $state) = @_;

  return 1 if not is_type('number', $data);
  return 1 if int($quotient) == $quotient and $quotient !~ /^-?Inf$/i;
  return E($state, 'value is not an even square multiple of %g', $schema->{multipleOf});

}

1;

