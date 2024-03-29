package Anki::Morphology;
use 5.14.0;
use utf8::all;
{ no warnings 'deprecated'; use Any::Moose; }
use Text::MeCab;
use Encode 'decode_utf8';
use Unicode::Normalize;
use DateTime;
use JSON ();

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
    default => sub { Text::MeCab->new($ENV{MECAB_USERDIC} && -r $ENV{MECAB_USERDIC} ? (userdic => $ENV{MECAB_USERDIC}) : ()) },
    lazy    => 1,
);

has intake => (
  is      => 'ro',
  isa     => 'Str',
  default => sub { $ENV{INTAKE_DATABASE} || "/var/opt/intake.sqlite" },
);

has readings => (
    is      => 'ro',
    isa     => 'HashRef[Str]',
    builder => '_build_readings',
    lazy    => 1,
);

has canto_readings => (
    is => 'ro',
    isa => 'HashRef[ArrayRef[Str]]',
    builder => '_build_canto_readings',
    lazy => 1,
);

has canto_kanji_readings => (
    is => 'ro',
    isa => 'HashRef[ArrayRef[ArrayRef[Str]|Num]]',
    builder => '_build_canto_kanji_readings',
    lazy => 1,
);

sub _build_readings {
    my $self = shift;
    my $readings = shift;

    for my $yomi ($self->anki->field_values("読み", "文")) {
        while ($yomi =~ /([^【】\s<>]+)【([^【】\s<>]+)】/g) {
	    $readings->{$1} = $2;
	}
    }

    for ($self->manual_japanese_vocabulary) {
      $readings->{$_->[1]} = $_->[2];
    }

    return $readings;
}

sub readings_for_word {
  my $self = shift;
  my $word = shift;

  my $readings = $self->readings;
  return $readings->{$word} if $readings->{$word};
  return;
}

sub readings_for {
    my $self            = shift;
    my $sentence        = shift;
    my $include_unknown = shift;

    my @readings;
    my %seen;
    my $yomi;
    my %added;
    my $manual_readings;

    NODE: for (my $node = $self->mecab->parse($sentence); $node; $node = $node->next) {
        my @fields = split ',', decode_utf8 $node->feature;
        my $surface = decode_utf8 $node->surface;
        my $dict = $fields[6];
        next unless ($dict||'') =~ /\p{Unified_Ideograph}/ || ($surface||'') =~ /\p{Unified_Ideograph}/;

        for my $word ($dict, $surface) {
            next if $seen{$word}++;

            if (my $reading = $self->readings->{$word}) {
                push @readings, [$word, $reading];
		$added{$dict} = 1;
		$added{$surface} = 1;
		next NODE;
            }
        }

	if ($include_unknown && !$added{$dict} && !$added{$surface}) {
          my $left = ($dict || '*') eq '*' ? $surface : $dict;
          my $right = ($fields[7] || $fields[8] || "") . "?";
          push @readings, [$left, $right];
          $added{$dict} = 1;
          $added{$surface} = 1;
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
        next if $dict eq '*' || $dict =~ /^[a-zA-Z ]*$/;

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

sub morphemes_by_date {
    my $self = shift;

    $self->_rescan_morphemes;

    my $sth = $self->knowndb->prepare("
        SELECT dictionary, added
        FROM morphemes
        ORDER BY added ASC
    ;");
    $sth->execute;

    my %by_date;
    while (my ($dict, $added) = $sth->fetchrow_array) {
	my $date = DateTime->from_epoch(epoch => $added)->ymd;
        push @{$by_date{$date}}, $dict;
    }
    return \%by_date;
}

sub _rescan_morphemes {
    my $self = shift;

    my $last_update = ($self->knowndb->selectrow_array("
        SELECT value
        FROM vars
        WHERE key = 'last_update';
    ;"))[0] || 0;

    my $last_new = int($self->anki->last_new_card / 1000);
    if ($last_new <= $last_update) {
      return;
    }

    warn "New cards, rescanning morphology...\n";

    my %i;

    my $sth = $self->knowndb->prepare("
        SELECT dictionary
        FROM morphemes
        ORDER BY added ASC
    ;");
    $sth->execute;

    my $added;
    while (my ($dict) = $sth->fetchrow_array) {
        $i{$dict}++;
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

    return;
}

sub known_morphemes {
    my $self = shift;

    $self->_rescan_morphemes;

    return $self->fast_known_morphemes;
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

sub _intake_vocabulary {
    my $self = shift;
    my $table = shift eq 'ja' ? 'japaneseVocabulary' : 'cantoneseVocabulary';
    my $dbh = DBI->connect("dbi:SQLite:dbname=" . $self->intake);
    $dbh->{sqlite_unicode} = 1;

    my $sth = $dbh->prepare("
      SELECT DATE(time, 'unixepoch'), word, reading, time
      FROM $table
      WHERE (_deleted IS NULL or _deleted=0)
      ORDER BY time DESC
    ");
    $sth->execute;

    my @vocabulary;

    while (my ($date, $word, $reading, $time) = $sth->fetchrow_array) {
      push @vocabulary, [$date, $word, $reading, $time];
    }

    return @vocabulary;
}

sub manual_japanese_vocabulary { shift->_intake_vocabulary('ja') }
sub manual_canto_vocabulary { shift->_intake_vocabulary('can') }

sub _build_canto_readings {
    my $self = shift;

    my %readings_for;

    for ($self->manual_canto_vocabulary) {
        my ($date, $word, $reading) = @$_;
        push @{ $readings_for{$word} }, $reading;
    }

    return \%readings_for;
}

sub _build_canto_kanji_readings {
    my $self = shift;

    my %readings;

    $self->anki->each_note(sub {
      my ($note) = @_;
      return if $note->has_tag('duplicate-kanji');

      my $cantonese = NFC($note->field('廣東話') or return);
      my $kanji = NFC($note->field('漢字'));

      my @readings = split ', ', $cantonese;

      $readings{$kanji} = [\@readings, $note->id];
    }, '漢字');

    return \%readings;
}

my %number_mappings = (
    0 => '零',
    1 => '一',
    2 => ['二', '兩'],
    3 => '三',
    4 => '四',
    5 => '五',
    6 => '六',
    7 => '七',
    8 => '八',
    9 => '九',
    10 => '十',
);
sub canto_number_mappings {
    my $self = shift;
    my $number = shift;

    my $readings = $number_mappings{$number} or return;
    return $readings if !ref($readings);
    return @$readings;
}

sub canto_morphemes_of {
    my $self = shift;
    my $line = shift;
    my $options = shift || {};
    my $best = delete $options->{best};
    my $allow_unknown = delete $options->{allow_unknown};
    my $include_alphanumeric = delete $options->{include_alphanumeric};

    confess "Unexpected options to canto_morphemes_of: " . join(', ', keys %$options) if %$options;

    my $word_regex = $include_alphanumeric ? qr/\p{Unified_Ideograph}|[a-zA-Z0-9]+/ : qr/\p{Unified_Ideograph}/;
    my @characters = $line =~ /$word_regex/g;
    my $readings = $self->canto_readings;

    my $recurse;
    $recurse = sub {
      return unless @_;

      my @parses;
      my @queue;

      for my $i (0 .. @_-1) {
        push @queue, [
          [],
          [@_[0..$i]],
          [@_[$i+1..@_-1]],
        ];
      }

      for my $i (0 .. @queue - 1) {
        local $_ = $queue[$i];
        my @preparse = @{ $_->[0] };
        my @candidate = @{ $_->[1] };
        my @rest = @{ $_->[2] };
        my $word = join '', @candidate;
        next if $readings->{$word};

        my @number_mappings = $self->canto_number_mappings($word);
        if (@number_mappings) {
          $queue[$i] = [
            [],
            [shift @number_mappings],
            \@rest,
          ];
          for my $mapping (@number_mappings) {
            push @queue, [
              [],
              [$mapping],
              \@rest,
            ];
          }
        }

        # 究、究⋯究竟發生了甚麼事呢⋯
        # 爸、爸爸很快便會來到！！
        # 彌、彌次郎⋯做得好啊！
        DUPE: for my $dupes (1 .. @candidate - 1) {
          for my $i (0 .. $dupes) {
            if ($candidate[0] ne $candidate[$i]) {
              next DUPE;
            }
          }

          push @queue, [
            [({word => $candidate[0], type => 'duplicate'}) x $dupes],
            [@candidate[$dupes..$#candidate]],
            \@rest,
          ];
        }

	# 庫啵啵！！
	# TODO: find more examples to generalize
	if ((join "", @candidate) eq '庫啵啵') {
          if (@rest) {
            push @parses, [@preparse, {word => "庫啵", type => "primary"}, {word => "啵", type => "duplicate"},, @$_] for $recurse->(@rest);
          }
          else {
            push @parses, [@preparse, {word => "庫啵", type => "primary"}, {word => "啵", type => "duplicate"},];
          }
	  next;
	}

        # 你鍾唔鍾意呢個呀
        # 你知不知道？
        if (@candidate >= 3 && $candidate[0] eq $candidate[2] && $candidate[1] =~ /^(?:唔|不)$/) {
          my $dupe = shift @candidate;
          my $mod = shift @candidate;
          $queue[$i] = [
            [{word => $dupe, type => 'modified'}, {word => $mod, type => 'modifier'}],
            \@candidate,
            \@rest,
          ];
        }
      }

      while (local $_ = shift @queue) {
        my @candidate = @{ $_->[1] };
        my $word = join '', @candidate;

        if (!$readings->{$word}) {
          next;
        }

        my @preparse = @{ $_->[0] };
        my @rest = @{ $_->[2] };

        if (@rest) {
          push @parses, [@preparse, {word => $word, type => 'primary'}, @$_] for $recurse->(@rest);
        }
        else {
          push @parses, [@preparse, {word => $word, type => 'primary'}];
        }
      }

      if (@parses == 0) {
        my $word = shift;

        if ($include_alphanumeric && $word =~ /[a-zA-Z0-9]/) {
          if (@_) {
            push @parses, [{word => $word, type => 'alphanumeric'}, @$_] for $recurse->(@_);
          }
          else {
            push @parses, [{word => $word, type => 'alphanumeric'}];
          }
        }
        elsif ($allow_unknown) {
          if (@_) {
            push @parses, [{word => $word, type => 'unknown'}, @$_] for $recurse->(@_);
          }
          else {
            push @parses, [{word => $word, type => 'unknown'}];
          }
        }
      }

      return @parses;
    };

    my @parses = $recurse->(@characters);

    # discard any unnecessary unknowns
    {
      my $discard_unknown = 0;
      for (@parses) {
        if ((grep { $_->{type} eq 'unknown' } @$_) == 0) {
          $discard_unknown = 1;
        }
      }

      if ($discard_unknown) {
        @parses = grep { (grep { $_->{type} eq 'unknown' } @$_) == 0 } @parses;
      }
    }

    # sort by fewest words required, then more primary words is better
    @parses = sort { @$a <=> @$b || (grep { $_->{type} eq 'primary' } @$b) <=> (grep { $_->{type} eq 'primary' } @$a) } @parses;

    if ($best) {
      return $parses[0];
    }

    my %seen;
    return grep { $seen{join "\0", map { $_->{word} } @$_}++ == 0} @parses;
}

sub best_canto_reading_for_sentence {
    my $self = shift;
    my $line = shift;

    my $morphemes = $self->canto_morphemes_of($line, { best => 1, allow_unknown => 1, include_alphanumeric => 1 });
    return $self->summarize_canto_morphemes($morphemes);
}

sub summarize_canto_morphemes {
    my $self = shift;
    my $morphemes = shift;
    my @results;
    my $vocab_only = 1;

    for my $morpheme (@$morphemes) {
        my $word = $morpheme->{word};
	my $word_reading;
	my $word_vocab = 1;

        if ($word =~ /^[a-zA-Z0-9]+$/) {
            $word_reading = $word;
        } else {
          my @readings = $self->canto_readings_for($word);

          if (@readings == 0) {
	      $word_vocab = 0 if $morpheme->{type} eq 'primary' || $morpheme->{type} eq 'unknown';
	      my @word_readings;
              for my $character (split '', $word) {
                  my @character_readings = $self->canto_kanji_readings_for($character);
	  	my $character_reading;
                  if (@character_readings == 0) {
                      $character_reading = $character . "??";
	              $word_vocab = 0;
                  } elsif (@character_readings > 1) {
                      $character_reading = $character_readings[0] . "?";
	              $word_vocab = 0;
                  } else {
                      $character_reading = $character_readings[0];
                  }
	  	push @word_readings, $character_reading;
              }
	      $word_reading = join " ", @word_readings;
          } elsif (@readings > 1) {
              $word_reading = $readings[0] . "?";
          } else {
              $word_reading = $readings[0];
          }
        }

	$vocab_only = 0 if !$word_vocab;
	push @results, $word_reading;
        $morpheme->{reading} = $word_reading;
	$morpheme->{known} = $word_vocab ? JSON::true : JSON::false;
    }

    my $result = join ' ', @results;
    return ($result, $vocab_only) if wantarray;
    return $result;
}

sub canto_readings_for {
    my $self = shift;
    my $word = shift;

    return @{ $self->canto_readings->{$word} || [] };
}

sub canto_kanji_readings_for {
    my $self = shift;
    my $kanji = shift;

    return @{ $self->canto_kanji_readings->{$kanji}[0] || [] };
}

sub canto_match_sentence_reading {
    my $self = shift;
    my $sentence = shift;
    my $sentence_reading = shift;

    my @parses = $self->canto_morphemes_of($sentence, { include_alphanumeric => 1 });
    if (!@parses) {
        my ($words) = $self->canto_morphemes_of($sentence, { best => 1, allow_unknown => 1, include_alphanumeric => 1 });
        return { no_parse => $words };
    }

    my @correct;
    my @misparses;
    PARSE: for my $parse (@parses) {
        my @sentence_readings = split ' ', $sentence_reading;
        my @sentence_readings_left = @sentence_readings;
        my @correct_words;
        my @correct_morphemes;
        my @correct_readings;

        WORD: for my $morpheme (@$parse) {
            my $type = $morpheme->{type};
            my $word = $morpheme->{word};

            if ($type eq 'duplicate' || $type eq 'modified') {
                my @kanji_readings = $self->canto_kanji_readings_for($word);
                for my $reading (@kanji_readings) {
                    if ($reading eq $sentence_readings_left[0]) {
                        push @correct_words, $word;
                        push @correct_morphemes, $morpheme;
                        push @correct_readings, $reading;
                        shift @sentence_readings_left;
                        next WORD;
                    }
                }

                push @misparses, {
                    word => $word,
                    expected => \@kanji_readings,
                    sentence_readings => \@sentence_readings,
                    sentence_readings_left => \@sentence_readings_left,
                };
                next PARSE;
            }
            elsif ($type eq 'alphanumeric') {
                if ($word eq $sentence_readings_left[0]) {
                    push @correct_words, $word;
                    push @correct_morphemes, $morpheme;
                    push @correct_readings, $word;
                    shift @sentence_readings_left;
                    next WORD;
                } else {
                    push @misparses, {
                      word => $word,
                      expected => [$word],
                      sentence_readings => \@sentence_readings,
                      sentence_readings_left => \@sentence_readings_left,
                    };
                    next PARSE;
                }
            }
            elsif ($type eq 'primary' || $type eq 'modifier') {
                my @word_readings = $self->canto_readings_for($word);
                for my $word_reading (@word_readings) {
                    my @word_reading = split ' ', $word_reading;
                    my @matches = grep { $word_reading[$_] eq ($sentence_readings_left[$_] || '') } 0 .. @word_reading - 1;
                    if (@matches == @word_reading) {
                        push @correct_words, $word;
                        push @correct_morphemes, $morpheme;
                        push @correct_readings, join ' ', @word_reading;
                        splice @sentence_readings_left, 0, scalar(@word_reading);
                        next WORD;
                    }
                }

                push @misparses, {
                  word => $word,
                  expected => \@word_readings,
                  sentence_readings => \@sentence_readings,
                  sentence_readings_left => \@sentence_readings_left,
                };

                next PARSE;
            }
            else {
                confess "produced unexpected type '$type' for $word";
            }
        }

        if (@sentence_readings_left == 0) {
            push @correct, {
                words => \@correct_words,
                morphemes => \@correct_morphemes,
                readings => \@correct_readings,
            };
        }
    }

    return {
      correct => \@correct,
      misparses => \@misparses,
    };
}

1;
