
require_relative "graph.rb"

class Synsets

  def initialize
    @synsets_hash = {}
    
  end
  
  #method to load file
  def load(synsets_file)
    
    File.open(synsets_file).each do |line|

      line.delete!("\n")

      if !!line.match(/^id:\s\d+\ssynset:\s([\w\.\'\/\-]+)(,[\w\.\'\/\-]+)*$/)
        words = line.split(Regexp.union(["id:","synset:"," ",","]))
                  .delete_if{|e| e.length == 0}
        #
        addSet(Integer(words[0]), words[1..words.size])
      else
        nil
      end
      
    end

    @synsets_hash
  end
  def addSet(synset_id, nouns)
    if synset_id >= 0 and (not nouns.empty?) and
      (not @synsets_hash.has_key?(synset_id))

      @synsets_hash.store(synset_id,nouns)
      true
      
    else
      false    
    end
  end  
  def lookup(synset_id)
    if @synsets_hash.has_key?(synset_id)
      @synsets_hash[synset_id]
    else
      []
    end
  end
  def findSynsets(to_find)

    
    if String === to_find

      id_found = []
      
      @synsets_hash.each do |key,value|
        if value.include?(to_find)
          id_found.append(key)
        end
      end

      id_found
      
    elsif Array === to_find and (not to_find.empty?)
      array_hash = {}

      # for each noun in the array, find if it exists
      # in the hash set, include the key in the
      # array_hash 
      to_find.each do |noun|
        @synsets_hash.each do |key,value|
          if value.include?(noun)
            array_hash.store(key,value)
          end
        end
      end

      array_hash
      
    else
      nil
    end
      
  end
end

###################
# Hypernyms Class #
###################
class Hypernyms
  
  def initialize
    @hypernyms_hash = {}
  end
  def load(hypernyms_file)

    invalid_lines = []
    line_num = 0;

    ## Scan each line in file
    File.open(hypernyms_file).each do |line|
      line_num += 1
      
      line.delete!("\n") #remove leading new line characters

      
      if line.match( /^from:\s\d+\sto:\s(\d+)(,[\d]+)*$/)
        # If format is correct, add the hypernym
        words = line.split(Regexp.union(["from:","to:"," ",","])
                  .delete_if{|e| e.length == 0}
        addSet(Integer(words[0]),words[1..words.size].map(&:to_i))      
      else
        # Get every invalid line
        invalid_lines.append(line_num)
      end
    end

    # Return
    if invalid_lines.empty?
      nil
    else
      invalid_lines
    end
  end  
  def addHypernym(source, destination)
    if source == destination or source < 0 or destination < 0
      false
    elsif @hypernyms_hash[destination] == nil
      @hypernyms_hash.store(destination,[source])
      true
    elsif Array === @hypernyms_hash[destination]
      if not  @hypernyms_hash[destination].include?(source)
        @hypernyms_hash[destination].append(source)
      end
      true
    end
    false
  end
  def lca(id1, id2)
    if @hypernyms_hash.has_key?(id1) and
       @hypernyms_hash.has_value?(id1) and
       @hypernyms_hash.has_key?(id2) and
       @hypernyms_hash.has_value?(id2)
    end
end

class CommandParser
  def initialize
    @synsets = Synsets.new
    @hypernyms = Hypernyms.new
  end

  def parse(command)
  
  end
end
