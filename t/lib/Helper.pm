use strictures 2;
# no package, so things defined here appear in the namespace of the parent.

use JSON::PP ();
use constant { true => JSON::PP::true, false => JSON::PP::false };

use JSON::MaybeXS;
my $encoder = JSON::MaybeXS->new(allow_nonref => 1, utf8 => 0);

# like sprintf, but all list items are JSON-encoded. assumes placeholders are %s!
sub json_sprintf {
  sprintf(shift, map +(ref($_) =~ /^Math::Big(Int|Float)$/ ? ref($_).'->new(\''.$_.'\')' : $encoder->encode($_)), @_);
}

1;
