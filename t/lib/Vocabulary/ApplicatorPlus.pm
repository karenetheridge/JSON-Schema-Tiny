package Vocabulary::ApplicatorPlus;

with 'JSON::Schema::Draft201909::Vocabulary';

sub vocabulary { 'https://github.com/karenetheridge/JSON-Schema-Draft201909/vocab/applicator_plus' }

sub keywords {
  qw(twoOf);
}

sub _traverse_keyword_twoOf {
  my ($self, $schema, $state) = @_;

}

sub _eval_keyword_twoOf {
  my ($self, $data, $schema, $state) = @_;

}

1;
