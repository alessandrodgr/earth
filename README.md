# EuclideAn Rhythm generaTor in Haskell 
A Haskell implementation of a [euclidean rhythm](http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf) generator

## Installation
This software requires [Euterpea](http://euterpea.com/download-and-installation/) library and a [MIDI synthesizer](http://www.euterpea.com/euterpea/setting-up-midi/) to run

Other dependencies are listed in [earth.cabal](earth.cabal), to install them run ```cabal build```

## Usage
```
cabal run rhythmFile
```
The input file specifies the rhythm in a JSON format. Here is an example of a cuban tresillo played on the E note of a slap bass:
```
{
  "tempo": {
    "numerator": 4,
    "denominator": 1
  },
  "tracks": [
    {
      "instrument": "SlapBass1",
      "note": "e",
      "octave": 2,
      "delay": 0,
      "beats": 3,
      "total": 8
    }
  ]
}
```
All [Euterpea's instruments](http://hackage.haskell.org/package/Euterpea-2.0.5/docs/src/Euterpea.Music.html#InstrumentName) are available, except ```CustomInstrument```

## Examples
More complex examples are provided in the [examples](examples) directory. They show more articulated rhythms made by mixing together simple euclidean rhythms played on different instruments, notes, octaves and onset (delay)
