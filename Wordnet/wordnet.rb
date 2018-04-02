

#TO DO
class Word
	
	attr_accessor:links
	attr_reader:nouns
	attr_reader:defs
	
	def initialize(arrOfNouns, link, arrOfDefs)
		@nouns = arrOfNouns
		@links = link
		@defs = arrOfDefs
	end
		
end


class WordNet

	attr_reader:graph
	
	def initialize(synsetsFile, hypernymsFile)
		@graph = Hash.new
		
		
		IO.foreach(synsetsFile) { |line|
		arrayOfLine = line.split(/,/) # split by commas
		
		id = arrayOfLine[0].to_i # turns id into integer
		nouns = arrayOfLine[1].split # array of nouns
		
		definition = arrayOfLine[2] # defintion which is last
		
		linkk = Array.new
		graph[id] = Word.new(nouns, linkk, definition) # create a new Word object

		}
		
		IO.foreach(hypernymsFile) { |line|
		arrayOfLine = line.split(/,/) # split by commas
		id = arrayOfLine.delete_at(0).to_i # turns id into integer
		
		arrayOfLine.each{|w| graph[id].links.push(w)} # add all the links to the graph
		}
	end 

	
	def isnoun(nouns)
		nounCount = 0
		nouns.each {|searchNoun| 
			graph.each_value {|valWord| 
				nounsAtVal = valWord.nouns
				nounsAtVal.each {|noun| if noun == searchNoun then nounCount += 1 end }
			} 
		}
		if nounCount == nouns.length
			puts "true"
		else	
			puts "false"
		end
	end
	
	def nouns
		numNouns = 0
		graph.each_value {|value| numNouns += value.nouns.length }
		puts numNouns
	end
	
	def edges
		numEdges = 0
		graph.each_value {|value| numEdges += value.links.length}
		puts numEdges
	end
	
	def length(v,w)
		shortestDist = +1.0/0.0
		v.each{|vInArray| 
			w.each{|wInArray|
				dist =  lengths(vInArray.to_i,wInArray.to_i) 
				if dist < shortestDist then shortestDist = dist end
			}
		}
		if shortestDist == +1.0/0.0 then return -1 
		else return shortestDist end
	end
	
	def lengths(v,w)
		vVert = graph[v]
		wVert = graph[w]
		
		if vVert.class == NilClass or wVert.class == NilClass then return +1.0/0.0 end
		if v == w then return 0 end
		
		vEdges = vVert.links
		wEdges = wVert.links
		
		distance = 1
		vEdgeAncestorsLength = Hash.new
		temp = []	
		
		while vEdges.any? do
			vEdges.each { |edge|
				if vEdgeAncestorsLength[edge.to_i] == nil then 
					vEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < vEdgeAncestorsLength[edge.to_i] then vEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks) # ancestors
			}
			vEdges = []
			temp.each {|e| vEdges.push(e)}
			temp = []	
			distance += 1
		end

		distance = 1
		wEdgeAncestorsLength = Hash.new
		while wEdges.any? do
			wEdges.each { |edge|
				if wEdgeAncestorsLength[edge.to_i] == nil then 
					wEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < wEdgeAncestorsLength[edge.to_i] then wEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks)
			}
			wEdges = []
			temp.each {|e| wEdges.push(e)}
			temp = []
			distance += 1
		end

		shortestDistance = -1
		vEdgeAncestorsLength.each_key {|ancestorID|
			if wEdgeAncestorsLength.has_key?(ancestorID) then 
				if (wEdgeAncestorsLength[ancestorID] + vEdgeAncestorsLength[ancestorID]) < shortestDistance or (shortestDistance == -1) then
					shortestDistance = (vEdgeAncestorsLength[ancestorID] + wEdgeAncestorsLength[ancestorID])
				end
			end
		}
		
		if vEdgeAncestorsLength.empty? == true then
			if wEdgeAncestorsLength.has_key?(v) == true then
				shortestDistance = wEdgeAncestorsLength[v]
			end
		end
		if wEdgeAncestorsLength.empty? == true then
			if vEdgeAncestorsLength.has_key?(w) == true then
				shortestDistance = vEdgeAncestorsLength[w]
			end
		end
		return shortestDistance
		
	end
	
	
	
	def ancestor(v,w)
		ancestorID = -1
		v.each{|vInArray| 
			w.each{|wInArray|
				ancestorID =  ancestors(vInArray.to_i,wInArray.to_i)
			}
		}
		puts ancestorID
	end
	
	def ancestors(v,w)
		vVert = graph[v]
		wVert = graph[w]
		
		if vVert.class == NilClass or wVert.class == NilClass then return -1 end
		
		vEdges = vVert.links
		wEdges = wVert.links
		
		distance = 1
		vEdgeAncestorsLength = Hash.new
		temp = []	
		
		while vEdges.any? do
			vEdges.each { |edge|
				if vEdgeAncestorsLength[edge.to_i] == nil then 
					vEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < vEdgeAncestorsLength[edge.to_i] then vEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks) # ancestors
			}
			vEdges = []
			temp.each {|e| vEdges.push(e)}
			temp = []	
			distance += 1
		end

		distance = 1
		wEdgeAncestorsLength = Hash.new
		while wEdges.any? do
			wEdges.each { |edge|
				if wEdgeAncestorsLength[edge.to_i] == nil then 
					wEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < wEdgeAncestorsLength[edge.to_i] then wEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks)
			}
			wEdges = []
			temp.each {|e| wEdges.push(e)}
			temp = []
			distance += 1
		end

		shortAncestorID = -1
		shortestDistance = -1
		
		# TESTING PURPOSES
		#puts vEdgeAncestorsLength
		#puts wEdgeAncestorsLength
		
		vEdgeAncestorsLength.each_key {|ancestorID|
			if wEdgeAncestorsLength.has_key?(ancestorID) and wEdgeAncestorsLength[ancestorID] != 0 then 
				if (wEdgeAncestorsLength[ancestorID] + vEdgeAncestorsLength[ancestorID]) < shortestDistance or (shortestDistance == -1) then
					shortestDistance = (vEdgeAncestorsLength[ancestorID] + wEdgeAncestorsLength[ancestorID])
					shortAncestorID = ancestorID
				end
			end
		}
		return shortAncestorID
		
	end
	
	
	def root(v,w)
	#puts v
	#puts w
	#puts graph
		shortestNoun = []
		shortestIds = []
		shortestIds[0] = -1
		vv = -1
		ww = -1
		graph.each {|key, value| 
			value.nouns.each {|noun| 
			if noun == v then vv = key end
			if noun == w then ww = key end
			}
		}
		if vv != -1 and ww != -1 then shortestIds = ancestorsRoot(vv,ww) end
		if vv == ww then shortestIds[0] = vv end
		if shortestIds.empty? == false then
		shortestIds.each {|id|
			graph[id].nouns.each {|noun|
				shortestNoun.push(noun) 
			}
		}
		end
		if shortestNoun.empty? == true then puts [""] 
		else 
			shortestNoun.sort!
			shortestNoun.each {|noun| print} end
		
	end
	
	
	def ancestorsRoot(v,w)
		vVert = graph[v]
		wVert = graph[w]
		
		if vVert.class == NilClass or wVert.class == NilClass then return -1 end
		
		vEdges = vVert.links
		wEdges = wVert.links
		
		distance = 1
		vEdgeAncestorsLength = Hash.new
		temp = []	
		
		vEdgeAncestorsLength[v] = 0
		while vEdges.any? do
			vEdges.each { |edge|
				if vEdgeAncestorsLength[edge.to_i] == nil then 
					vEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < vEdgeAncestorsLength[edge.to_i] then vEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks) # ancestors
			}
			vEdges = []
			temp.each {|e| vEdges.push(e)}
			temp = []	
			distance += 1
		end

		distance = 1
		wEdgeAncestorsLength = Hash.new
		wEdgeAncestorsLength[w] = 0
		while wEdges.any? do
			wEdges.each { |edge|
				if wEdgeAncestorsLength[edge.to_i] == nil then 
					wEdgeAncestorsLength[edge.to_i] = distance 
				else 
					if distance < wEdgeAncestorsLength[edge.to_i] then wEdgeAncestorsLength[edge.to_i] = distance end
				end
				theLinks = graph[edge.to_i].links
				temp.concat(theLinks)
			}
			wEdges = []
			temp.each {|e| wEdges.push(e)}
			temp = []
			distance += 1
		end

		shortAncestorID = []
		shortestDistance = -1
		
		# TESTING PURPOSES
		#puts vEdgeAncestorsLength
		#puts wEdgeAncestorsLength
		
		vEdgeAncestorsLength.each_key {|ancestorID|
			if wEdgeAncestorsLength.has_key?(ancestorID) and wEdgeAncestorsLength[ancestorID] != 0 then 
				if (wEdgeAncestorsLength[ancestorID] + vEdgeAncestorsLength[ancestorID]) <= shortestDistance or (shortestDistance == -1) then
					shortestDistance = (vEdgeAncestorsLength[ancestorID] + wEdgeAncestorsLength[ancestorID])
					shortAncestorID.push(ancestorID)
				end
			end
		}
		if shortAncestorID.empty? == false then return shortAncestorID 
		else return [] 
		end
		
	end
	
	
	
	def outcast(arrOfWords)
		nounToId = Hash.new #key: noun    value: array of ids
		distHash = Hash.new #hash of key: id    value: length 
		arrOfWords.each {|noun|
			nounToId[noun] = getNounId(noun)
		}
		curLen = 0
		nounToId.each_value {|id| 
			nounToId.each_value {|oVal|
				curLen += (length(id, oVal) * length(id, oVal))
			}
			distHash[id] = curLen
			curLen = 0
		}
		
		#puts distHash
		
		maxLen = 0
		maxId= 0
		pos = 0
		count = 0
		distHash.each {|id, length|
			if length > maxLen == true then 
				maxLen = length 
				maxId = id 
				pos = count
			end
			count += 1
		}
		maxNoun = arrOfWords[pos]
		puts maxNoun
		
		
	end
	
	def getNounId(nounIn)
		idArray = []
		graph.each {|key, value|
			value.nouns.each {|noun|
				if noun == nounIn then idArray.push(key) end
			}
		}
		return idArray
	end
	
	

end

	

if ARGV.length < 3 || ARGV.length >5
  fail "usage: wordnet.rb <synsets file> <hypersets file> <command> <filename>"
end

synsets_file = ARGV[0]
hypernyms_file = ARGV[1]
command = ARGV[2]
fileName = ARGV[3]

commands_with_0_input = %w(edges nouns)
commands_with_1_input = %w(outcast isnoun)
commands_with_2_input = %w(length ancestor)



case command
when "root"
	file = File.open(fileName)
	v = file.gets.strip
	w = file.gets.strip
	file.close
    wordnet = WordNet.new(synsets_file, hypernyms_file) 
    r =  wordnet.send(command,v,w)  
    r.each{|w| print "#{w} "}
    
when *commands_with_2_input 
	file = File.open(fileName)
	v = file.gets.split(/\s/).map(&:to_i)
	w = file.gets.split(/\s/).map(&:to_i)
	file.close
    wordnet = WordNet.new(synsets_file, hypernyms_file)
    puts wordnet.send(command,v,w)  
when *commands_with_1_input 
	file = File.open(fileName)
	nouns = file.gets.split(/\s/)
	file.close
    wordnet = WordNet.new(synsets_file, hypernyms_file)
    puts wordnet.send(command,nouns)
when *commands_with_0_input
	wordnet = WordNet.new(synsets_file, hypernyms_file)
	puts wordnet.send(command)
else
  fail "Invalid command"
end
  
  