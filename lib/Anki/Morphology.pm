package Anki::Morphology;
use 5.14.0;
use utf8::all;
use Any::Moose;
use Text::MeCab;
use Encode 'decode_utf8';

use Anki::Database;
use Anki::Corpus;

# ABSTRACT: MeCab etc for my Anki::Corpus and Anki::Database

has anki => (
    is      => 'ro',
    isa     => 'Anki::Database',
    default => sub { Anki::Database->new },
    lazy    => 1,
);

has corpus => (
    is      => 'ro',
    isa     => 'Anki::Corpus',
    default => sub { Anki::Corpus->new(morphology => shift) },
    lazy    => 1,
);

has knowndb => (
    is      => 'ro',
    lazy    => 1,
    default => sub {
        my $self = shift;

        my $dbh = DBI->connect("dbi:SQLite:dbname=$ENV{HOME}/.knownmorph.sqlite");
        $dbh->{sqlite_unicode} = 1;
        $dbh
    },
);

has mecab => (
    is      => 'ro',
    isa     => 'Text::MeCab',
    default => sub { Text::MeCab->new(-r $ENV{MECAB_USERDIC} ? (userdic => $ENV{MECAB_USERDIC}) : ()) },
    lazy    => 1,
);

has readings => (
    is      => 'ro',
    isa     => 'HashRef[Str]',
    default => sub { {} },
);

sub readings_for {
    my $self     = shift;
    my $sentence = shift;

    my @readings;
    my %seen;
    my $yomi;

    NODE: for (my $node = $self->mecab->parse($sentence); $node; $node = $node->next) {
        my @fields = split ',', decode_utf8 $node->feature;
        my $surface = decode_utf8 $node->surface;
        my $dict = $fields[6];
        next unless ($dict||'') =~ /\p{Han}/ || ($surface||'') =~ /\p{Han}/;

        for my $word ($dict, $surface) {
            next if $seen{$word}++;

            if (!$self->readings->{$word}) {
                $yomi //= [ $self->anki->field_values("読み", "文") ];
                for my $readings (@$yomi) {
                    my ($reading) = $readings =~ /(?:>|\n|^)\Q$word\E【(.*?)】/;
                    if ($reading) {
                        $self->readings->{$word} = $reading;
                        last;
                    }
                }
            }

            push @readings, [$word, $self->readings->{$word}];
            last;
        }
    }

    return @readings if wantarray;
    return join "\n", map { "$_->[0]【$_->[1]】" } @readings;
}

sub morphemes_of {
    my $self     = shift;
    my $sentence = shift;

    my @morphemes;

    for (my $node = $self->mecab->parse($sentence); $node; $node = $node->next) {
        # possibly the end of a sentence
        my $surface = decode_utf8($node->surface or last);

        my @fields = split ',', decode_utf8 $node->feature;
        my $type = $fields[0];
        next if $type eq '記号'; # symbol

        my $dict = $fields[6];

        # not a Japanese word
        next if $dict eq '*';

        push @morphemes, {
            surface    => $surface,
            type       => $type,
            dictionary => $dict,
            reading    => $fields[8],
        };
    }

    return @morphemes;
}

sub known_morphemes_uncached {
    my $self = shift;

    my %i;
    my @keep;

    for my $sentence ($self->anki->field_values("日本語", "文")) {
        for my $morpheme ($self->morphemes_of($sentence)) {
            my $dict = $morpheme->{dictionary};
            push @keep, $dict if $i{$dict}++ == 0;
        }
    }

    return @keep;
}

sub fast_known_morphemes {
    my $self = shift;

    my $sth = $self->knowndb->prepare("
        SELECT dictionary
        FROM morphemes
        ORDER BY added ASC
    ;");
    $sth->execute;

    my @keep;
    while (my ($dict) = $sth->fetchrow_array) {
        push @keep, $dict;
    }
    return @keep;
}

sub known_morphemes {
    my $self = shift;

    my $last_update = ($self->knowndb->selectrow_array("
        SELECT value
        FROM vars
        WHERE key = 'last_update';
    ;"))[0] || 0;

    my $last_new = int($self->anki->last_new_card / 1000);

    return $self->fast_known_morphemes
        if $last_new <= $last_update;

    warn "New cards, rescanning morphology...\n";

    my %i;
    my @keep;

    my $sth = $self->knowndb->prepare("
        SELECT dictionary, added
        FROM morphemes
        ORDER BY added ASC
    ;");
    $sth->execute;

    my $added;
    while (((my $dict), $added) = $sth->fetchrow_array) {
        push @keep, $dict if $i{$dict}++ == 0;
    }

    $added ||= 0; # first run

    my @new;
    $self->anki->each_card(sub {
        my ($card) = @_;

        return if $card->suspended;
        my $sentence = $card->field('日本語');
        return if !$sentence;

        for my $morpheme ($self->morphemes_of($sentence)) {
            my $dict = $morpheme->{dictionary};
            next if $i{$dict}++;

            push @keep, $dict;
            push @new, $dict;

            my $added = int($card->id / 1000);

            $self->knowndb->do("
                INSERT INTO morphemes
                ('dictionary', 'added', 'source')
                VALUES (?, ?, ?)
            ", {}, $dict, $added, $card->note_id);
        }
    });

    if (@new > 10) {
        my $more = () = splice @new, 10;
        warn "New morphemes: " . join('、', @new) . ", and $more others...\n";
    }
    elsif (@new > 0) {
        warn "New morphemes: " . join('、', @new) . "\n";
    }

    $self->knowndb->begin_work;
    $self->knowndb->do("
        DELETE FROM vars
        WHERE key = 'last_update'
    ;");
    $self->knowndb->do("
        INSERT INTO vars (key, value) VALUES ('last_update', $last_new);
    ;");
    $self->knowndb->commit;

    return @keep;
}

sub morpheme_frequency {
    my $self = shift;
    my $noisy = 1;
    my ($i, $e) = (0, 0);

    my %f;

    my $sth = $self->corpus->prepare("SELECT morphemes FROM sentences;");
    $sth->execute;

    while (my ($morphemes) = $sth->fetchrow_array) {
        say 2**$e++ if $noisy && ++$i == 2**$e;
        $f{ $_ }++ for split ' ', $morphemes;
    }

    return %f;
}

sub common_words_for {
    my $self   = shift;
    my $substr = shift;

    my $sth = $self->corpus->prepare("SELECT morphemes FROM sentences WHERE morphemes LIKE ?");
    $sth->execute("%$substr%");

    my $qr = qr/\Q$substr\E/;

    my %words;
    while (my ($morphemes) = $sth->fetchrow_array) {
        for my $morpheme (split ' ', $morphemes) {
            $words{$morpheme}++
                if $morpheme =~ $qr;
        }
    }

    return \%words;
}

1;
