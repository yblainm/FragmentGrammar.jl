# old goldstandard
const goldstandard_file = "C:\\Users\\Daniel\\Documents\\Bitbucket\\musical-grammars\\data\\annotations\\Goldstandard.txt"
const chroma = Dict("C"=>0,"D"=>2,"E"=>4,"F"=>5,"G"=>7,"A"=>9,"B"=>11)

function pitch_class(note_name::AbstractString)
    m = match(r"([A-G])(b*|#*)", note_name)
    if contains(m[2], "#")
        chroma[m[1]] + length(m[2])
    else
        chroma[m[1]] - length(m[2])
    end
end

function translate_goldstandard_notation1(chord::AbstractString)
    m = match(r"([A-G]b*#*)(.*)", chord)
    string(pitch_class(m[1]), m[2])
end

function translate_goldstandard_notation1(sequence::AbstractVector)
    map(translate_goldstandard_notation1, sequence)
end

const chord_form_dict = Dict(""=>"maj", "-"=>"min", "%"=>"hdim", "o"=>"dim")

function translate_goldstandard_notation2(chord::AbstractString)
    # println(chord)
    m = match(r"([A-G]b*#*)(.*)", chord)
    # println(m)
    string(pitch_class(m[1]), chord_form_dict[m[2]])
end

function translate_goldstandard_notation2(sequence::AbstractVector)
    map(translate_goldstandard_notation2, sequence)
end

function produce_goldstandard_sequences()
    lines = map(strip, readlines(goldstandard_file))
    for line in lines
        if !isempty(line) && line[1] == '\$' && line[end] == '\$'
            if contains(line[2:end-1], "maj")
                produce(translate_goldstandard_notation1(split(line[2:end-1])))
            else
                produce(translate_goldstandard_notation2(split(line[2:end-1])))
            end
        end
    end
end

function read_goldstandard()
    collect(@task(produce_goldstandard_sequences()))
end

#new treebank
const treebank_chord_form_dict = Dict(""=>"maj", "7"=>"maj", "^7"=>"maj", "6"=>"maj",
                                      "m"=>"min", "m7"=>"min", "m6"=>"min",
                                      "%7"=>"hdim", "o"=>"dim", "o7"=>"dim", "+"=>"aug")

function translate_treebank_notation(chord::AbstractString)
    m = match(r"([A-G]b*#*)(.*)", chord)
    string(pitch_class(m[1]), treebank_chord_form_dict[m[2]])
end

function translate_treebank_notation(chords::AbstractVector)
    map(translate_treebank_notation, chords)
end

function right_branching_tree(num_leafs::Int)
    if num_leafs == 1
        TreeNode("x")
    else
        n = TreeNode("x")
        insert_child!(n, right_branching_tree(num_leafs - 1))
        insert_child!(n, TreeNode("x"))
        n
    end
end

function read_tree(treebank_file::AbstractString)
    chomp(readlines(treebank_file)[4])[3:end]
end

function read_treebank_trees()
    dir = joinpath(dirname(@__FILE__), "..", "..", "trees")
    files = ["afternoon_in_paris.tsv", "all_of_me.tsv", "alone_together.tsv", "anthropology.tsv", "a_night_in_tunisia.tsv"]
    # files = ["afternoon_in_paris.tsv", "all_of_me.tsv"]
    trees = TreeNode{String}[]
    for file in files
        t = tree(read_tree(joinpath(dir, file)))
        for n in t
            n.data = translate_treebank_notation(n.data)
        end
        push!(trees, t)
    end
    trees
end

function read_goldstandard_trees_1()
    trees = map(tree,
    ["[Cmaj[Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmaj]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmaj]]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmaj[Cmaj][Cmaj]]]]][Cmaj[Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Dmin[Amaj][Dmin]][Gmaj]][Cmaj]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmaj]]]]"
    "[Cmaj[Cmaj[Cmaj][Gmaj[Dmin[Amaj[Emaj][Amaj]][Dmin]][Gmaj[Dmaj[Amin[Emaj[Bmin][Emaj]][Amin][Dmaj]][Gmaj[Dmin][Gmaj]]]]]][Cmaj[Cmaj[Cmaj][Cmaj[Gmaj[Dmin[Amaj[Emaj][Amaj]][Dmin]][Gmaj[Fmaj][Gmaj[Fmin][Gmaj[Dmin[Amaj[Emin][Amaj]][Dmin]][Gmaj]]]]][Cmaj]]][Gmaj[Dmin[Ebdim][Dmin]][Gmaj]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Gmaj[Gmaj[Dmin[Amin[Emaj[Bhdim[Fmaj][Bhdim]][Emaj]][Amin]][Dmin[Ebmaj][Dmin]]][Gmaj]][Gmaj[Dmin[Amin[Emin][Amin]][Dmin]][Gmaj]]]][Cmaj[Cmaj[Gmaj[Dmaj[Amaj[Emin][Amaj]][Dmaj[Amin][Dmaj]]][Gmaj[Dmin][Gmaj]]][Cmaj]][Cmaj[Gmaj[Gmaj[Dmin[Amin[Emaj[Bhdim[Fmaj][Bhdim]][Emaj]][Amin]][Dmin[Ebmaj][Dmin]]][Gmaj]][Gmaj[Dmin[Amin[Emin][Amin]][Dmin]][Gmaj]]][Cmaj]]]][Cmaj[Gmaj[Amin[Gmaj[Dhdim[Amin][Dhdim]][Gmaj]][Amin[Emin][Amin]]][Gmaj[Fmaj[Cmaj[Gmaj[Dhdim][Gmaj]][Cmaj]][Fmaj]][Gmaj[Dmin[Dmin[Amaj[Emin[Bmaj[F#min][Bmaj]][Emin]][Amaj]][Dmin]][Dmin[Dmin[Amaj][Dmin]][Dmin[Amaj][Dmin]]]][Gmaj[Abmaj][Gmaj]]]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Gmaj[Dmin[Amin[Emaj[Bhdim[Fmaj][Bhdim]][Emaj]][Amin]][Dmin[Ebmaj][Dmin]]][Gmaj]][Gmaj[Dmin[Amin[Emin][Amin]][Dmin]][Gmaj]]][Cmaj]]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Gmaj[Dmaj[Amin][Dmaj]][Gmaj[Gmaj][Gmaj]]]][Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Cmaj[Gmaj][Cmaj]]]][Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Gmaj[Dmaj[Amin][Dmaj]][Gmaj[Gmaj][Gmaj]]]][Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Gmaj[Gmaj][Gmaj[Abmaj][Gmaj]]]]]][Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Cmaj[Gmaj[Dmaj[Amin][Dmaj]][Gmaj]][Cmaj]]]]"
    "[Cmin[Cmin[Cmin[Cmin][Cmin[Gmaj][Cmin]]][Gmaj[Abmaj][Gmaj]]][Cmin[Cmin[Cmin][Cmin[Gmaj][Cmin]]][Cmin[Gmaj[Abmaj][Gmaj]][Cmin]]]]"
    "[Cmaj[Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Fmaj][Cmaj]]]][Cmaj[Cmaj[Fmaj][Cmaj]][Cmaj[Gmaj[Dmin[Amaj[Emin][Amaj]][Dmin]][Gmaj]][Cmaj]]]]"
    "[Cmaj[Cmaj[Cmaj][Cmaj[Bbmaj][Cmaj]]][Cmaj[Dbmaj[Dmaj[Ebmaj[Bbmaj][Ebmaj]][Dmaj]][Dbmaj]][Cmaj]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Gmaj[Dmaj][Gmaj[Dmin][Gmaj]]][Cmaj]]][Cmaj[Cmaj][Cmaj[Gmaj[Dmaj][Gmaj[Dmin][Gmaj]]][Cmaj]]]][Cmaj[Cmaj[Gmaj[Fmaj][Gmaj[Dmaj][Gmaj[Dmin][Gmaj]]]][Cmaj[Dbmaj][Cmaj]]][Cmaj[Gmaj[Dmaj][Gmaj[Dmin][Gmaj]]][Cmaj[Cmaj][Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]]]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj][Amin[Emaj[Bhdim][Emaj]][Amin]]][Cmaj[Cmaj[Bbmaj[Fmaj[Cmaj[Gmin][Cmaj]][Fmaj]][Bbmaj]][Cmaj]][Gmaj[Dmaj[Amin][Dmaj]][Gmaj[Dmin][Gmaj]]]]][Cmaj[Cmaj[Cmaj[Cmaj][Amin[Emaj[Bhdim][Emaj]][Amin]]][Cmaj[Bbmaj[Fmaj[Cmaj[Gmin][Cmaj]][Fmaj]][Bbmaj]][Cmaj]]][Cmaj[Gmaj[Dmaj][Gmaj[Dmin[Amaj[Emin[Emin[D#dim][Emin]][Emin[Fmaj][Emin]]][Amaj]][Dmin]][Gmaj]]][Cmaj[Cmaj][Cmaj[Gmaj][Cmaj]]]]]]"
    "[Cmaj[Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Fmin][Gmaj[Dmin][Gmaj[Abmaj[Ebmaj][Abmaj]][Gmaj]]]]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Fmin][Gmaj[Dmin][Gmaj]]]][Cmaj]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj[Dbmaj][Cmaj[Gaug][Cmaj[Cmaj][Cmaj]]]][Cmaj[Galt[Fmin[Cmaj[Ghdim[Dmaj[Cmaj][Dmaj]][Ghdim]][Cmaj]][Fmin[Fmin][Fmin]]][Galt[Dhdim][Galt]]][Cmaj[Cmaj][Cmaj]]]][Cmaj[Galt[Fmin[Cmaj[Ghdim[Dmaj[Cmaj][Dmaj]][Ghdim]][Cmaj]][Fmin[Fmin][Fmin]]][Galt[Dhdim][Galt]]][Cmaj[Cmaj][Cmaj[Cmaj][Cmaj]]]]][Cmaj[Gmaj[Bbmaj[Fmaj[Cmin[Cmin][Cmin]][Fmaj]][Bbmaj[Bbmaj][Bbmaj]]][Gmaj[Abmaj[Abmaj][Abmaj]][Gmaj[Dmin][Gmaj]]]][Cmaj[Gaug[Fmin[Cmaj[Ghdim][Cmaj]][Fmin[Fmin][Fmin]]][Gaug[Dbmaj][Gaug]]][Cmaj[Cmaj[Cmaj][Cmaj[Cmaj][Cmaj]]][Cmaj[Dmaj][Cmaj]]]]]]"
    "[Cmaj[Cmaj[Cmaj][Cmaj[Cmin[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmin]][Cmaj[Gmaj[Dhdim][Gmaj]][Cmaj]]]][Cmaj[Cmaj[Gmaj[Gmaj[Dmin][Gmaj]][Gmaj[Dmin[Ebmaj[Emin][Ebmaj]][Dmin]][Gmaj]]][Cmaj]][Cmaj[Cmaj[Gmaj[Abmaj[Ebmaj[Bbmaj[Fmaj[Cmin][Fmaj]][Bbmaj]][Ebmaj[Bbmin][Ebmaj]]][Abmaj]][Gmaj[Dmin][Gmaj]]][Cmaj]][Cmaj[Cmaj[Gmaj[Gmaj[Dmin][Gmaj]][Gmaj[Dmin[Ebmaj[Emin][Ebmaj]][Dmin]][Gmaj]]][Cmaj]][Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]]]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Fmaj][Cmaj]]][Cmaj[Fmaj][Cmaj]]][Cmaj[Gmaj][Cmaj[Fmaj][Cmaj]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj][Amin[Emaj[Bhdim][Emaj]][Amin]]][Gmaj[Dmaj[Amaj[Bbmaj[Fmaj[Cmaj[Gmin][Cmaj]][Fmaj]][Bbmaj]][Amaj[Emin][Amaj]]][Dmaj]][Gmaj[Dmin][Gmaj]]]][Cmaj[Cmaj[Cmaj][Amin[Emaj[Bhdim][Emaj]][Amin]]][Gmaj[Abmaj[Ebmin[Bbmaj[Fmaj[Cmaj[Gmin][Cmaj]][Fmaj]][Bbmaj]][Ebmin[Emin][Ebmin]]][Abmaj]][Gmaj[Dmin][Gmaj]]]]]"
    "[Cmaj[Cmaj[Cmaj][Cmaj[Bbmaj[Fmin][Bbmaj]][Cmaj]]][Cmaj[Cmaj[Gmaj[Abmaj[Ebmaj[Bbmin][Ebmaj]][Abmaj]][Gmaj[Dmaj[Amin][Dmaj]][Gmaj[Dmin][Gmaj]]]][Cmaj]][Dbmaj[Abmaj[Ebmaj][Abmaj]][Dbmaj]]]]"
    "[Cmaj[Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Gmaj[Dmin[Amin][Dmin]][Gmaj]][Cmaj]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Gmaj[Gmaj[Dmin][Gmaj]][Gmaj[Dmin[Amaj[Emin][Amaj]][Dmin]][Gmaj]]]]][Gmaj[Gmaj[Dmaj[Ebmaj[Ebmaj][Ebmaj[Bbmaj[Fmin][Bbmaj]][Ebmaj[Ebmaj][Ebmaj]]]][Dmaj]][Gmaj]][Gmaj[Dmin[Ebmaj[Emin][Ebmaj]][Dmin]][Gmaj]]]][Cmaj[Cmaj[Cmaj[Cmaj][Cmaj[Gmaj[Dmin[Amin][Dmin]][Gmaj]][Cmaj]]][Cmaj[Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]][Gmaj[Gmaj[Dmin][Gmaj]][Gmaj[Dmin[Amaj[Emin][Amaj]][Dmin]][Gmaj]]]]][Cmaj[Cmaj[Dmin[Fmaj[Cmaj[Gmin][Cmaj]][Fmaj]][Dmin]][Cmaj]][Cmaj[Cmaj[Gmaj[Dmin[Amin][Dmin]][Gmaj]][Cmaj]][Cmaj[Gmaj[Dmin][Gmaj]][Cmaj]]]]]]"])
    for i in 1:length(trees)
        trees[i] = map(translate_goldstandard_notation1, trees[i])
    end
    trees
end

function read_parsable_goldstandard_trees_1()
    map(tree, ["[0maj[0maj[0maj[7maj[2min][7maj]][0maj]][0maj[7maj[2min[9maj][2min]][7maj]][0maj]]][0maj[0maj[7maj[2min][7maj]][0maj]][0maj[7maj[8maj[3maj[10maj[5maj[0min][5maj]][10maj]][3maj[10min][3maj]]][8maj]][7maj[2min][7maj]]][0maj]]]]",
    "[0min[0min[0min[0min][0min[7maj][0min]]][7maj[8maj][7maj]]][0min[0min[0min][0min[7maj][0min]]][0min[7maj[8maj][7maj]][0min]]]]",
    "[0maj[0maj[0maj[7maj[2min][7maj]][0maj]][0maj[0maj[7maj[2min][7maj]][0maj]][0maj[5maj][0maj]]]][0maj[0maj[5maj][0maj]][0maj[7maj[2min[9maj[4min][9maj]][2min]][7maj]][0maj]]]]",
    "[0maj[0maj[0maj][0maj[10maj][0maj]]][0maj[1maj[2maj[3maj[10maj][3maj]][2maj]][1maj]][0maj]]]",
    "[0maj[0maj[0maj[0maj][0maj[7maj[2maj][7maj[2min][7maj]]][0maj]]][0maj[0maj][0maj[7maj[2maj][7maj[2min][7maj]]][0maj]]]][0maj[0maj[7maj[5maj][7maj[2maj][7maj[2min][7maj]]]][0maj[1maj][0maj]]][0maj[7maj[2maj][7maj[2min][7maj]]][0maj[0maj][0maj[7maj[2min][7maj]][0maj]]]]]]",
    "[0maj[0maj[0maj][0maj[0min[7maj[8maj[3maj[10maj[5maj[0min][5maj]][10maj]][3maj[10min][3maj]]][8maj]][7maj[2min][7maj]]][0min]][0maj[7maj[2hdim][7maj]][0maj]]]][0maj[0maj[7maj[7maj[2min][7maj]][7maj[2min[3maj[4min][3maj]][2min]][7maj]]][0maj]][0maj[0maj[7maj[8maj[3maj[10maj[5maj[0min][5maj]][10maj]][3maj[10min][3maj]]][8maj]][7maj[2min][7maj]]][0maj]][0maj[0maj[7maj[7maj[2min][7maj]][7maj[2min[3maj[4min][3maj]][2min]][7maj]]][0maj]][0maj[7maj[2min][7maj]][0maj]]]]]]",
    "[0maj[0maj[0maj[0maj][0maj[5maj][0maj]]][0maj[5maj][0maj]]][0maj[7maj][0maj[5maj][0maj]]]]"])
end

function read_goldstandard_trees_2()
    trees = map(tree,
    # ["[C[C[C[C[C[C][A-]][C[Db[Ab][Db]]C]][C[Db[D[Eb][D]]Db][C[C][A-]]]][C[Db[Ab][Db]]C]][C[C[G[F[F[C[G[B][G[Ab][G]]][C]][F[F][D-]]][F[Gb[Db][Gb]][F]]][G[Ab][G]]][C[Db][C[C][A-]]]][C[C[Db[Ab][Db]][C]][C[Db][C[G[Ab][G]][C]]]]]]",
    ["[C-[C-[C-[C-[G[D%][G]][C-]][Eb[Bb[F-[C][F-]][Bb]][Eb]]][C-[C-[G[D%][G]][C-]][C-[G[F-][G[Ab][G]]][C-]]]][C-[G[D%[A][D%]][G]][C-[C-[C-[C-[G[D%][G]][C-]][Eb[Bb[F-[C][F-]][Bb]][Eb]]][C-[C-[G[D%][G]][C-]][C-[G[F-][G[Ab][G]]][C-]]]] [C-[G[Ab[A][Ab]][G]][C-]]]]]",
    "[C-[C-[C-[Eb[Bb[F-][Bb]][Eb]][C-[G[D%[Ab][D%]][G]][C-]]][C-[Eb[Bb[F-[C][F-]][Bb]][Eb]][C-[G[D%[Ab][D%]][G]][C-]]]][C-[C-[C-[G[D%][G]][C-]][Eb[Bb[F-][Bb]][Eb]]][C-[C-[G[D%][G]][C-]][C-[G[D%[Ab[Bbb[Bb-[Cb][Bb-]][Bbb]][Ab]][D%]][G]][C-]]]]]",
    "[C-[C-[C-[C-[C-[C-][C-[G[D%][G]][C-]]][C-[C-[G[D%][G]][C-]][Eb[Bb[F-][Bb]][Eb]]]][C-[Eb[Bb[F-[Eo][F-[C][F-]]][Bb]][Eb]][C-[G[D%[Ab][D%]][G]][C-]]]][C-[C-[C-[G[D%][G]][C-]][C-[G[D%][G]][C-]]][C-[G[F-[C[G[D%][G]][C[G%][C]]][F-]][G[D%[Ab[C-[G[F-][G[D%][G]]][C-]][Ab]][D%]][G]]][C-]]]][C-[C-[C-[F-][C-]][C-[F-][C-]]][C-[G-[F-][G-]][C-]]]]",
    "[C-[C-[C-[C-][C-[G[F-][G[D%][G]]][C-]]][C-[Db[Ab[Eb-][Ab]][Db]][C-[G[D%][G]][C-]]]][C-[G[D%][G]][C-]]]",
    "[C[C[C][C[C[G[F[C[G-[D[A-[E[B%][E]][A-]][D]][G-]][C]][F]][G[Ab[A[Bb[F-][Bb]][A[E-][A]]][Ab[Eb-][Ab]]][G[D-][G]]]][C]][A-]]][C[G[D-][G]][C]]]",
    # "[C[C[C[C][C[Bb[D-[A[Bb][A]][D-]][Bb[F-][Bb]]][C[G[G[D-[A-[E-][A-]][D-]][G]][G[D[A-[E[B%][E]][A-]]D]][G[D-][G]]][C]]]][C[Bb[D-[A[Bb][A]][D-]][Bb[F-][Bb]]][C[G[D-[A-[A-[E-][A-]][A-[E-[F[F#%][F]][E-]][A-]]][D-]][G]][C[C][C[F][C]]]]]][C[G[G[G[D-[A-[E-][A-]][D-]][G]][G[D-[A-[E-][A-]][D-]][G]]][G[D-[A-[E-][A-]][D-]][G]]][C[C][C[F][C]]]]]",
    "[C-[C-[C-][C-[F-][C-]]][C-[Db[D][Db]][C-]]]",
    "[C[C[C[C[G[D-[A-][D-]][G]][C]][A-[E[B%[F][B%]][E]][A-]]][C[C[G[D-[A][D-]][G]][C]][C[G[D-][G]][C]]]][C[C[C[G[D-[A-[E[B-][E]][A-]][D-]][G]][C]][A-[E[B%[F][B%]][E]][A-]]][C[C[G[D-[A][D-]][G]][C]][C[G[F[C][F]][G]][C]]]]]",])
    for i in 1:length(trees)
        # println(i)
        trees[i] = map(translate_goldstandard_notation2, trees[i])
    end
    trees
end

function read_parsable_goldstandard_trees_2()
    map(tree, [
    "[0min[0min[0min[0min[7maj[2hdim][7maj]][0min]][3maj[10maj[5min[0maj][5min]][10maj]][3maj]]][0min[0min[7maj[2hdim][7maj]][0min]][0min[7maj[5min][7maj[8maj][7maj]]][0min]]]][0min[7maj[2hdim[9maj][2hdim]][7maj]][0min[0min[0min[0min[7maj[2hdim][7maj]][0min]][3maj[10maj[5min[0maj][5min]][10maj]][3maj]]][0min[0min[7maj[2hdim][7maj]][0min]][0min[7maj[5min][7maj[8maj][7maj]]][0min]]]][0min[7maj[8maj[9maj][8maj]][7maj]][0min]]]]]",
    "[0min[0min[0min[0min][0min[7maj[5min][7maj[2hdim][7maj]]][0min]]][0min[1maj[8maj[3min][8maj]][1maj]][0min[7maj[2hdim][7maj]][0min]]]][0min[7maj[2hdim][7maj]][0min]]]",
    "[0maj[0maj[0maj][0maj[0maj[7maj[5maj[0maj[7min[2maj[9min[4maj[11hdim][4maj]][9min]][2maj]][7min]][0maj]][5maj]][7maj[8maj[9maj[10maj[5min][10maj]][9maj[4min][9maj]]][8maj[3min][8maj]]][7maj[2min][7maj]]]][0maj]][9min]]][0maj[7maj[2min][7maj]][0maj]]]",
    "[0min[0min[0min][0min[5min][0min]]][0min[1maj[2maj][1maj]][0min]]]",
    "[0maj[0maj[0maj[0maj[7maj[2min[9min][2min]][7maj]][0maj]][9min[4maj[11hdim[5maj][11hdim]][4maj]][9min]]][0maj[0maj[7maj[2min[9maj][2min]][7maj]][0maj]][0maj[7maj[2min][7maj]][0maj]]]][0maj[0maj[0maj[7maj[2min[9min[4maj[11min][4maj]][9min]][2min]][7maj]][0maj]][9min[4maj[11hdim[5maj][11hdim]][4maj]][9min]]][0maj[0maj[7maj[2min[9maj][2min]][7maj]][0maj]][0maj[7maj[5maj[0maj][5maj]][7maj]][0maj]]]]]",
    ])
end

# read_goldstandard_trees_2()

function train!(grammar, trees)
    for tree in trees
        forest = run_chartparser(tree, grammar)
        add_obs!(grammar, best_tree(forest))
    end
    grammar
end

function single_fold_cross_validation(trees, make_grammar=make_jazz_grammar, with_training=true)
    precision_vals = zeros(Float64, length(trees))
    for (i,test_tree) in enumerate(trees)
        grammar = make_grammar()
        if with_training
            train!(grammar, [trees[1:i-1];trees[i+1:end]])
        end
        @time forest  = run_chartparser(leaf_data(test_tree), grammar)
        precision_vals[i] = precision(best_tree(forest), test_tree)
    end
    precision_vals
end

function single_fold_cross_validation_baseline(trees)
    precision_vals = zeros(Float64, length(trees))
    for (i,test_tree) in enumerate(trees)
        baseline_tree = right_branching_tree(length(leafs(test_tree)))
        precision_vals[i] = precision(baseline_tree, test_tree)
    end
    precision_vals
end

# function single_fold_cross_validation(trees)
#     precision_vals = Float64[]
#     recall_vals = Float64[]
#     precision_base = Float64[]
#     recall_base = Float64[]
#
#     for (i,test_tree) in enumerate(trees)
#         # grammar = make_jazz_grammar()
#         grammar = make_plain_PCFG_jazz_grammar()
#
#         # training
#         for tree in [trees[1:i-1];trees[i+1:end]]
#             @time forest = run_chartparser(tree, grammar)
#             add_obs!(grammar, best_tree(forest))
#         end
#
#         # evaluating
#         @time forest  = run_chartparser(leaf_data(test_tree), grammar)
#         parsed_tree   = best_tree(forest)
#         baseline_tree = right_branching_tree(length(leafs(test_tree)))
#         push!(precision_vals, precision(parsed_tree,   test_tree))
#         push!(recall_vals,       recall(parsed_tree,   test_tree))
#         push!(precision_base, precision(baseline_tree, test_tree))
#         push!(recall_base,       recall(baseline_tree, test_tree))
#     end
#     precision_vals, recall_vals, precision_base, recall_base
# end

function parse_leaf_data(trees)
    grammar = make_jazz_grammar(Int)
    # grammar = make_plain_PCFG_jazz_grammar(BigInt)
    for k in 1:2
        println("id\tlength\ttime_in_seconds\t number_of_parses")
        for (i,t) in enumerate(trees)
            chords = leaf_data(t)
            tic()
            forest = run_chartparser(chords, grammar)
            time = toq()
            @assert is_complete(forest)
            println(i, "\t", length(chords), "\t", time, "\t", score(forest))
        end
    end
end

function make_parsable_trees()
    parsable_trees = [
        read_treebank_trees();
        read_parsable_goldstandard_trees_1();
        read_parsable_goldstandard_trees_2()
    ]
    [parsable_trees[1:6]; parsable_trees[8:end]]
end

# parse_leaf_data(parsable_trees)

# p,r,pb,rb = single_fold_cross_validation(parsable_trees)

# parse_trees(parsable_trees)
