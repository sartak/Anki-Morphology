package Anki::Morphology;
use utf8::all;
use Any::Moose;
use Text::MeCab;
use Encode 'decode_utf8';

has anki => (
    is      => 'ro',
    isa     => 'Anki::Database',
    default => sub { Anki::Database->new },
    lazy    => 1,
);

has corpus => (
    is      => 'ro',
    isa     => 'Anki::Corpus',
    default => sub { Anki::Corpus->new },
    lazy    => 1,
);

has mecab => (
    is      => 'ro',
    isa     => 'Text::MeCab',
    default => sub { Text::MeCab->new },
    lazy    => 1,
);

sub readings_for {
    my $self     = shift;
    my $sentence = shift;

    my @readings;
    my %seen;

    NODE: for (my $node = $self->mecab->parse($sentence); $node; $node = $node->next) {
        my @fields = split ',', decode_utf8 $node->feature;
        my $surface = decode_utf8 $node->surface;
        my $dict = $fields[6];
        next unless $dict =~ /\p{Han}/;

        for my $word ($dict, $surface) {
            next if $seen{$word}++;
            my $sth = $self->anki->prepare("
                select fields.value
                from fields
                    join fieldModels on (fields.fieldModelId = fieldModels.id)
                    join models on (fieldModels.modelId = models.id)
                    join cards on (cards.factId = fields.factId)
                where
                    models.name is '文'
                    and fieldModels.name like '%読み%'
                    and cards.type > 0
                    and (
                        fields.value like ?
                        or fields.value like ?
                        or fields.value like ?
                    )
                    limit 1;
            ");
            $sth->execute("$word【%", "%\n$word【%", "%<br>$word【%");
            my ($readings) = $sth->fetchrow_array;
            next unless $readings;

            my ($reading) = $readings =~ /(?:<br>|\n|^)\Q$word\E【(.*?)】/;
            push @readings, [$word, $reading];
            last;
        }
    }
    return @readings if wantarray;
    return join "\n", map { "$_->[0]【$_->[1]】" } @readings;
}

1;
