module Composition.Note
	( NoteStructure (..),
	)
where

import Composition.Notelength (Notelength)
import Composition.Pitch
	( Pitch (..), 
		Pitchable
	)
import Numeric.Natural (Natural)

data NoteStructure
	= Note Notelength Pitch
	| Pause Notelength
	| Sequence [NoteStructure]
	| Group [NoteStructure]
	deriving (Show)
	