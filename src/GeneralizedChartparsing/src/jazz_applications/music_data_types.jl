#################
### Key class ###
#################

immutable Key
  root    :: ModInt{12}
  ismajor :: Bool
end

const key_dict = TwoWayDict(
    Key(0,  true) => "C",
    Key(1,  true) => "Db",
    Key(2,  true) => "D",
    Key(3,  true) => "Eb",
    Key(4,  true) => "E",
    Key(5,  true) => "F",
    Key(6,  true) => "Gb",
    Key(7,  true) => "G",
    Key(8,  true) => "Ab",
    Key(9,  true) => "A",
    Key(10, true) => "Bb",
    Key(11, true) => "B",
    Key(0, false) => "c",
    Key(1, false) => "c#",
    Key(2, false) => "d",
    Key(3, false) => "eb",
    Key(4, false) => "e",
    Key(5, false) => "f",
    Key(6, false) => "f#",
    Key(7, false) => "g",
    Key(8, false) => "g#",
    Key(9, false) => "a",
    Key(10,false) => "bb",
    Key(11,false) => "b")

Key(root::ModInt{12}, mode::AbstractString) =
    Key(root, mode == "major" ? true : false)
Key(root::Int, mode::AbstractString) =
    Key(ModInt{12}(root), mode == "major" ? true : false)
Key(s::AbstractString) = key_dict[s]

show(io::IO, k::Key) = print(io, key_dict[k])
root(key::Key) = key.root
ismajor(key::Key) = key.ismajor
isminor(k::Key) = !ismajor(k)
mode(k::Key)    = ismajor(k) ? "major" : "minor"
change_mode(k::Key) = Key(root(k), !ismajor(k))

function uses_flats(k::Key)
    if ismajor(k)
        root(k) * 7 > ModInt{12}(5)
    else
        (root(k) + 3) * 7 > ModInt{12}(5)
    end
end

+(k::Key, i) = Key(root(k) + i, ismajor(k))
-(k::Key, i) = Key(root(k) - i, ismajor(k))

###########################
### Scalae degree class ###
###########################

immutable ScaleDeg
    deg :: ModInt{7}
end

const roman_dict = TwoWayDict(
    "I"  => ModInt{7}(0),
    "II" => ModInt{7}(1),
    "III"=> ModInt{7}(2),
    "IV" => ModInt{7}(3),
    "V"  => ModInt{7}(4),
    "VI" => ModInt{7}(5),
    "VII"=> ModInt{7}(6))

ScaleDeg(str::String) = ScaleDeg(roman_dict[str])

numeral(s::ScaleDeg) = roman_dict[s.deg]
show(io::IO, s::ScaleDeg) = print(io, numeral(s))

+(s::ScaleDeg, i::Int) = ScaleDeg(s.deg + i)
-(s::ScaleDeg, i::Int) = ScaleDeg(s.deg - i)

const major_pattern = (0,2,4,5,7,9,11)
const minor_pattern = (0,2,3,5,7,8,10)

function root(s::ScaleDeg, k::Key) :: ModInt{12}
    pattern = ismajor(k) ? major_pattern : minor_pattern
    pattern[s.deg] + root(k)
end

const natural_tone_dict = TwoWayDict(
    "C" => ModInt{12}(0),
    "D" => ModInt{12}(2),
    "E" => ModInt{12}(4),
    "F" => ModInt{12}(5),
    "G" => ModInt{12}(7),
    "A" => ModInt{12}(9),
    "B" => ModInt{12}(11))

function spelled_root(s::ScaleDeg, k::Key) :: String
    pc = root(s, k) # pitch class of the root
    if pc in major_pattern # then it is the pitch class of a natural tone
        natural_tone_dict[pc]
    elseif uses_flats(k)
        natural_tone_dict[pc+1] * "b"
    else # key uses sharps
        natural_tone_dict[pc-1] * "#"
    end
end

const major_modes = ("major","minor","minor","major","major","minor","minor")
const minor_modes = ("minor","minor","major","minor","minor","major","major")
mode(s::ScaleDeg, k::Key) = (ismajor(k) ? major_modes : minor_modes)[s.deg]

modulate(s::ScaleDeg, k::Key) = Key(root(s, k), mode(s, k))

const major_triad_forms = ("maj","min","min","maj","maj","min","dim")
const minor_triad_forms = ("min","hdim","maj","min","min","maj","maj")
chord_form(s::ScaleDeg, k::Key) = (ismajor(k) ? major_triad_forms : minor_triad_forms)[s.deg]

terminal(s::ScaleDeg, k::Key, form::String=chord_form(s)) = string(Int(root(s, k)), form)

######################
### Category class ###
######################

abstract type AbstractCategory end

const chord_form_classification = Dict(
    "13b9" => "maj",
    "7sus4" => "maj",
    "o7" => "dim",
    "7b9" => "maj",
    "sus4" => "sus4",
    "m11" => "min",
    "m" => "min",
    "7" => "maj",
    "9#11" => "maj",
    "%7" => "hdim",
    "^7#11" => "maj",
    "13" => "maj",
    "+7" => "aug",
    "^6" => "maj",
    "+^7" => "aug",
    "9" => "maj",
    "^9#11" => "maj",
    "+" => "aug",
    "" => "maj",
    "m9" => "min",
    "m7" => "min",
    "^13" => "maj",
    "mb6" => "min",
    "11" => "maj",
    "13#9" => "maj",
    "o" => "dim",
    "^7" => "maj",
    "13" => "maj",
    "m6" => "min",
    "m6" => "min",
    "7b13" => "maj",
    "+9" => "maj",
    "7alt" => "alt",
    "7#11" => "maj",
    "9" => "maj",
    "13#11" => "maj",
    "7b5" => "maj",
    "7b9" => "maj",
    "m^7" => "min",
    "^9" => "maj"
)

immutable Terminal <: AbstractCategory
    symbol :: String
    root   :: ModInt{12}
    form   :: String
end
==(t1::Terminal, t2::Terminal) = t1.root == t2.root && t1.form == t2.form
hash(t::Terminal) = hash("Terminal", hash(t.root, hash(t.form)))

Terminal(s::ScaleDeg, k::Key, form::String=chord_form(s, k)) =
    Terminal(spelled_root(s, k) * form, root(s, k), form)
function Terminal(str::AbstractString)
    if ismatch(r"^(\d+)(.*)", str)
        m = match(r"^(\d*)(.*)", str)
        Terminal(str, parse(m[1]), m[2])
    elseif ismatch(r"^([A-G][b#]*)(.*)", str)
        m = match(r"^([A-G][b#]*)(.*)", str)
        pc = pitch_class(m[1])
        form = chord_form_classification[m[2]]
        Terminal(str, pc, form)
    end
end
show(io::IO, t::Terminal) = print(io, t.symbol)

function make_terminals(scaledegs, key)
   lst = split(scaledegs)
   [Terminal(ScaleDeg(String(s)),Key(key)) for s in lst]
end

immutable Category <: AbstractCategory
  scaledeg   :: ScaleDeg
  key        :: Key
  isdomsubst :: Bool
end
Category(str::String, k::Key, b::Bool) = Category(ScaleDeg(str), k, b)
Category(deg::ScaleDeg, str::String, b::Bool) = Category(deg, Key(str), b)
Category(str1::String, str2::String, b::Bool) = Category(ScaleDeg(str1), Key(str2), b)

show(io::IO, cat::Category) =
    print(io, scaledeg(cat), "_{", key(cat), "}", isdomsubst(cat) ? "T" : "F")

scaledeg(cat::Category) = cat.scaledeg
key(cat::Category) = cat.key
isdomsubst(cat::Category) = cat.isdomsubst

keyroot(cat::Category) = root(key(cat))
keymode(cat::Category) = mode(key(cat))
root(cat::Category) = root(scaledeg(cat), key(cat))
modulate(cat::Category) = modulate(scaledeg(cat), key(cat))
change_mode(cat::Category) = Category(scaledeg(cat), change_mode(key(cat)), isdomsubst(cat))

Terminal(cat::Category) = Terminal(scaledeg(cat), key(cat))
Terminal(cat::Category, form::AbstractString) = Terminal(scaledeg(cat), key(cat), form)

notdomsubst(cat::Category) = Category(scaledeg(cat), key(cat), false)

# diatonic transposition
transpose(cat::Category, i) = Category(scaledeg(cat) + i, key(cat), isdomsubst(cat))
