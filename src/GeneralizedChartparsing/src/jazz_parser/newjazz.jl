include("newtypes.jl")

# using DataFrames
# importall Base

########################
### TwoWayDict class ###
########################

function bijective_invert{K,V}(d::Dict{K,V})
    result =  Dict{V,K}()
    for (k, v) in d
        result[v] = k
    end
    result
end

type TwoWayDict{K,V}
    first  :: Dict{K, V}
    second :: Dict{V, K}
end

TwoWayDict{K,V}(first::Dict{K,V}) = TwoWayDict(first, bijective_invert(first))
TwoWayDict(pairs::Vararg) = TwoWayDict(Dict(pairs))
getindex{K,V}(d::TwoWayDict{K,V}, k::K) = d.first[k]
getindex{K,V}(d::TwoWayDict{K,V}, v::V) = d.second[v]

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
    "VII"=> ModInt{7}(6)
)

ScaleDeg(str::String) = ScaleDeg(roman_dict[str])

==(s::ScaleDeg, str::String) = numeral(s) == str
==(str::String, s::ScaleDeg) = s == str

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
    "B" => ModInt{12}(11)
)

function pitch_class(spelled_pitch_class::AbstractString)
    m = match(r"([A-G])(#*)(b*)", spelled_pitch_class)
    natural_tone_dict[String(m[1])] + length(m[2]) - length(m[3])
end

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
### JazzCategory class ###
######################

abstract type AbstractJazzCategory end

immutable JazzTerminal <: AbstractJazzCategory
    symbol :: String
    root   :: ModInt{12}
    form   :: String
end
==(t1::JazzTerminal, t2::JazzTerminal) = t1.root == t2.root && t1.form == t2.form
hash(t::JazzTerminal, h::UInt) = hash(hash("JazzTerminal", hash(t.root, hash(t.form))), h)

JazzTerminal(s::ScaleDeg, k::Key, form::String=chord_form(s, k)) =
    JazzTerminal(spelled_root(s, k) * form, root(s, k), form)

function JazzTerminal(str::AbstractString)
    if ismatch(r"^(\d+)(.*)", str)
        m = match(r"^(\d*)(.*)", str)
        JazzTerminal(str, parse(m[1]), m[2])
    elseif ismatch(r"^([A-G][b#]*)(.*)", str)
        m = match(r"^([A-G][b#]*)(.*)", str)
        pc = pitch_class(m[1])
        form = m[2]
        JazzTerminal(str, pc, form)
    end
end
show(io::IO, t::JazzTerminal) = print(io, t.symbol)

function make_terminals(scaledegs, key)
   lst = split(scaledegs)
   [JazzTerminal(ScaleDeg(String(s)),Key(key)) for s in lst]
end

immutable JazzCategory <: AbstractJazzCategory
  scaledeg   :: ScaleDeg
  key        :: Key
end
JazzCategory(str::String, k::Key) = JazzCategory(ScaleDeg(str), k)
JazzCategory(deg::ScaleDeg, str::String) = JazzCategory(deg, Key(str))
JazzCategory(str1::String, str2::String) = JazzCategory(ScaleDeg(str1), Key(str2))

show(io::IO, cat::JazzCategory) =
    print(io, scaledeg(cat), "_{", key(cat), "}")

scaledeg(cat::JazzCategory) = cat.scaledeg
key(cat::JazzCategory) = cat.key

keyroot(cat::JazzCategory) = root(key(cat))
keymode(cat::JazzCategory) = mode(key(cat))
root(cat::JazzCategory) = root(scaledeg(cat), key(cat))
spelled_root(cat::JazzCategory) = spelled_root(scaledeg(cat), key(cat))
modulate(cat::JazzCategory) = modulate(scaledeg(cat), key(cat))
change_mode(cat::JazzCategory) = JazzCategory(scaledeg(cat), change_mode(key(cat)))

JazzTerminal(cat::JazzCategory) = JazzTerminal(scaledeg(cat), key(cat))
JazzTerminal(cat::JazzCategory, form::AbstractString) = JazzTerminal(scaledeg(cat), key(cat), form)

# diatonic transposition
Base.transpose(cat::JazzCategory, i) = JazzCategory(scaledeg(cat) + i, key(cat))

const chord_form_dict = Dict(
    ":maj7" => "^7",
    "7" => "7",
    ":min7" => "m7",
    "h7" => "%7",
    "7#9" => "7",
    "7b9" => "7",
    ":maj7#5" => "^7",
    "" => "^",
    "6" => "6",
    "9" => "7",
    ":min6" => "m6",
    "o7" => "o7",
    "+" => "+",
    "7#5" => "7",
    ":maj7#11" => "^7",
    "69" => "6",
    ":min7b5" => "%7",
    "9sus" => "sus",
    "7b9#5" => "7",
    "7b13" => "7",
    "7alt" => "7",
    "7sus" => "sus",
    ":min" => "m",
    "7#9b5" => "7",
    ":min9" => "m",
    "7#11" => "7",
    "13" => "7",
    "h" => "%7",
    "^9" => "^7",
    ":min:maj7" => "m^7",
    "11" => "7",
    ":min11" => "m7",
    "o" => "o7",
    "9#5" => "7",
    "7b9sus" => "sus",
    "7b5" => "7",
    "add9" => "7",
    "7b9b5" => "7",
    "^" => "^",
    "9#11" => "7",
    "maj7#11" => "^7",
    "maj7#5" => "^7",
    "6;" => "6",
    ":maj7;" => "^7",
    ":min6;" => "m6",
    "7#9#5" => "7",
    "13b9" => "7",
    ":minb6" => "m7",
    "7#11;" => "7",
    "13sus" => "sus",
    ":maj7#11;" => "^7",
    ":min69" => "m6",
    "maj7" => "^7",
    "^9#11" => "^7",
    ";" => "^",
    "7;" => "7",
    "13#11" => "7",
    "7b9#9" => "7",
    ":maj9" => "^7",
    "13#11;" => "7",
    "7#9#11" => "7",
    ":maj" => "^",
    "7susadd3" => "sus",
    ":min;" => "m",
    "7b9b13" => "7",
    "7b9#11" => "7",
    "7#9;" => "7",
    ":min:maj7;" => "m^7",
    ":min9;" => "m7",
    ":7" => "7",
    "769" => "7",
    ":7sus" => "sus",
    "dim7" => "o7",
    "sus" => "sus",
    "7b13sus" => "sus",
    "9;" => "7",
    "9#11;" => "7",
    ":maj7#9#11;" => "^7",
    "7#9#5;" => "7",
    "69;" => "6",
    ":maj7#9#11" => "^7",
    "9b5" => "7",
    "^;" => "^"
)

function read_iReal_songs(dir="../iRealPro/iRb_thru/")
    files = readdir(dir)

    pieces_with_meter_change = [
        "conferenceofthebirds.jazz",
        "homecoming.jazz",
        "howmyheartsings.jazz",
        "imeanyou.jazz",
        "isayalittleprayerforyou.jazz",
        "joshua.jazz",
        "lookoflove.jazz",
        "midnightattheoasis.jazz",
        "tellmeabedtimestory.jazz",
        "walktall.jazz"
    ]

    pieces_with_strange_meter = [ # non-strange meters are "4/4", "3/4", and "6/4"
        "litha.jazz",             # 6/8 meter
        "looktotherainbow.jazz",  # 3/2. meter
        "takefive.jazz",          # 5/4
        "thebalance.jazz"         # 5/4
    ]

    iReal_songs = DataFrame(
        file = String[],
        title = String[],
        composers = Vector{String}[],
        year = Int[],
        form = String[],
        meter = String[],
        key = String[],
        chords = DataFrame[]
    )

    for (i, file) in enumerate(setdiff(files, [pieces_with_meter_change; pieces_with_strange_meter]))
        if ismatch(r"^\.", file) continue end # skip hidden files

        lines = readlines(joinpath(dir, file))

        title = ""
        composers = String[]
        year = 0
        form = ""
        meter = ""
        keystring = ""

        bar = 1
        beat = 1.0
        chords = DataFrame(
            bar=Int[],
            beat=Float64[],
            duration=Float64[],
            spelledroot=String[],
            pcroot=Int[],
            chordform=String[],
            slash=Union{Void, String}[],
            alternative=Union{Void, String}[]
        )

        for line in lines
            if ismatch(r"[!*]", line) # meta data line
                if ismatch(r"^!!!OTL", line)
                    @assert title == ""
                    title = match(r"^!!!OTL: (.*)", line)[1]
                elseif ismatch(r"^!!!COM", line)
                    push!(composers, match(r"^!!!COM\d?:?\t? ?(.*)", line)[1])
                elseif ismatch(r"^!!!ODT", line)
                    @assert year == 0
                    year = parse(match(r"^!!!ODT: (.*)", line)[1])
                elseif ismatch(r"^\*>\[", line)
                    @assert form == ""
                    form = match(r"^\*>(.*)", line)[1]
                elseif ismatch(r"^\*M", line)
                    # @assert meter == ""
                    meter = match(r"^\*M(.*)", line)[1]
                elseif ismatch(r"^\*(.*):", line)
                    # @assert keystring == ""
                    keystring = match(r"^\*(.*):", line)[1]
                end
            elseif ismatch(r"^=", line) # new bar
                bar += 1
                beat = 1.0
            elseif ismatch(r"^\dr", line) # rest line
                duration = 4/parse(match(r"^(\d)r", line)[1])
                beat += duration
            elseif ismatch(r"^\d", line) # note line
                @assert ismatch(r"(\d+)(\.*)([A-G])([-#]?)([^/]*)([A-G][-#]?)?(\(.*\))?", line)
                m = match(r"(\d+)(\.*)([A-G])([-#]?)([^/()]*)([A-G][-#]?)?(\(.*\))?", line)
                duration, punktuation, chordroot, accidential, chordform, slash, alternative =
                    4/parse(m[1]), m[2], m[3], replace(m[4], "-", "b"), m[5], m[6], m[7]
                for p in punktuation
                    duration += duration / 2
                end
                pcroot = natural_tone_dict[String(chordroot)] +
                    (contains(accidential, "#") ? length(accidential) : -length(accidential))
                spelledroot = chordroot*accidential
                push!(chords,
                    (bar, beat, duration, spelledroot, pcroot.val, chord_form_dict[chordform], slash, alternative)
                )

                beat += duration
            end
        end
        push!(iReal_songs, (file, title, composers, year, form, meter, keystring, chords))
    end
    iReal_songs
end

iReal_songs = read_iReal_songs();
@show number_chord_symbols = size(vcat(iReal_songs[:chords]...))

function terminals(iReal_chord_df)
    Vector(
        JazzTerminal.(
            iReal_chord_df[:spelledroot] .* iReal_chord_df[:chordform],
            iReal_chord_df[:pcroot],
            iReal_chord_df[:chordform]
        )
    )
end

@show terminals(iReal_songs[4, :chords])[1:13]

const major_keys = [Key(r,"major") for r in 0:11]
const minor_keys = [Key(r,"minor") for r in 0:11]
const scale_degrees = [ScaleDeg(s) for s in 0:6]

categories(scale_deg::String) = categories([scale_deg])

function categories(scale_degrees::Vector{String}=["I", "II", "III", "IV", "V", "VI", "VII"]; without="")
    [
        JazzCategory(s, k)
        for s in ScaleDeg.(setdiff(scale_degrees, [without]))
        for k in vcat(major_keys, minor_keys)
    ]
end

function init_jazz_grammar(dependent_components=cat -> (scaledeg(cat), keymode(cat)))
    prolongation = CFRule(categories(), :prolongation) do c
        [c, c]
    end

    diatonic_prep = CFRule(categories(without="IV"), :diatonic_prep) do c
        [transpose(c, 4), c]
    end
    dominant_prep = CFRule(categories(without="I"), :dominant_prep) do c
        [JazzCategory("V", modulate(c)), c]
    end
    plagal_prep = CFRule(categories("I"), :plagal_prep) do c
        [JazzCategory("IV", key(c)), c]
    end

    modulation = CFRule(categories(without="I"), :modulation) do c
        [JazzCategory("I", modulate(c))]
    end
    mode_change = CFRule(categories("I"), :mode_change) do c
        [change_mode(c)]
    end

    diatonic_subs = CFRule(categories(["I", "II", "V"]), :diatonic_subs) do c
        if ismajor(key(c))
            if scaledeg(c) == "I"
                [JazzCategory("VI", key(c))]
            elseif scaledeg(c) == "II"
                [JazzCategory("IV", key(c))]
            else
                [JazzCategory("VII", key(c))]
            end
        else
            if scaledeg(c) == "I"
                [JazzCategory("III", key(c))]
            elseif scaledeg(c) == "II"
                [JazzCategory("IV", key(c))]
            else
                [JazzCategory("VII", key(c))]
            end
        end
    end

    for i in (3, 6, 9)
        eval(quote
            $(Symbol("octa_dom_subs", i)) = CFRule(categories("V"), Symbol("octa_dom_subs", $i)) do c
                [JazzCategory(scaledeg(c), key(c) + $i)]
            end
        end)
    end

    category_rules = [
        prolongation,
        diatonic_prep,
        dominant_prep,
        plagal_prep,
        modulation,
        mode_change,
        diatonic_subs,
        octa_dom_subs3,
        octa_dom_subs6,
        octa_dom_subs9
    ];

    pitchclass(c::JazzCategory) = root(modulate(c))

    chord_form_dict_ = Dict(
        "major" => Dict(
            "I"   => ["^", "^7", "6"],
            "II"  => ["m", "m7"],
            "III" => ["m", "m7"],
            "IV"  => ["^", "^7", "6"],
            "V"   => ["^", "7", "+", "sus"],
            "VI"  => ["m", "m7"],
            "VII" => ["o7", "%7"]
        ),
        "minor" => Dict(
            "I"   => ["m", "m7", "m^7"],
            "II"  => ["%7"],
            "III" => ["^", "^7", "6"],
            "IV"  => ["m", "m7"],
            "V"   => ["m", "m7", "^", "7", "+", "sus"],
            "VI"  => ["^", "^7", "6"],
            "VII" => ["^", "7", "+", "sus"]
        )
    )

    terminal_rules = push!(
        [
            CFRule([JazzCategory(s, Key(r, mode)) for r in 0:11], Symbol(form, "_termination")) do c
                [JazzTerminal(spelled_root(c) * form, pitchclass(c), form)]
            end
            for mode in ("major", "minor")
            for s in ("I", "II", "III", "IV", "V", "VI", "VII")
            for form in chord_form_dict_[mode][s]
        ],
        CFRule(JazzCategory.("V", major_keys), Symbol("m6_termination")) do c
            [JazzTerminal(spelled_root(transpose(c, 4)) * "m6", pitchclass(transpose(c, 4)), "m6")]
        end
    )

    CFGrammar(category_rules, terminal_rules, categories("I"), dependent_components)
end

iReal_songs[1:10, [:file, :title, :composers, :year, :form, :meter, :key]]

# grammar = init_jazz_grammar()
# chords = terminals(iReal_songs[4, :chords])[1:13]
# @time forest = run_chartparser(chords, grammar, cyclic=false); iscomplete(forest)
# @time forest = run_chartparser(chords, grammar, cyclic=true); iscomplete(forest)
# println(sample_tree(forest))
# iscomplete(forest)
# score(forest)
# category.(heads(forest))
# println(sample_tree(forest))
# @time category.(heads(run_chartparser(chords, grammar)))
#
# terminals(iReal_songs[4, :chords])
