
# Basic Idea

  This is going to be a relatively simple compiler for the .nanki filetype. the
  basic idea is that the anki notes are going to be placed into this file, where
  it'll parsed and compiled into multiple anki compatable .txt files (This may
  change if I can find a better way of importing).

# Syntax

```
  default_card = Basic;
  deck = example_deck;
  default_tags = Cool_Tags;

  #[Basic]
  This is an example of a basic card type; It also has a field for the back;
  You_Can_Put_Tags Here As_well

  #[Cloze]
  Clozes work in a similar way {c1:Word}; The back can be filled;

  #[Cloze]
  or empty {c1:Word};;

  #[Basic_Reverse]
  White space and newlines are
  handled

  correctly; And work

  as expected;

  #[]
  Default card types are available through empty `#[]`;;

  #[Four_Field_Card_Type]
  Multiple fields are handled as expected;
  Field two;
  Field three;
  Field four;
  Tags Also_newlines_after_semi-colons_are_removed_at_compile_time

  #[]
  You can overwrite tags as well
  ; Default tags will not be applied
  <;> Tag_one

  default_card = Cloze;

  #[]
  Variables are defined {c1:procedurally}; Meaning values can be redefined in
  later parts of the file;

```

# Features

  Multiple compilation flags:
  - Seperator char (TODO)
  - Default card (TODO)
  - Deck (TODO)

  Compilation warnings (TODO)

  Syntax highlighting (TODO)

  Neovim Plugin (TODO)

# Technicals

## Compilation Process

   Source Code -> Lexical Analyzer -> Syntax Analyzer -> Semantic Analyzer ->
   cards_.txt
