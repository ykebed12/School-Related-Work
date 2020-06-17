require_relative "graph.rb"

class Synsets

  attr_accessor :synsets_hash

  def initialize
    @synsets_hash = {}
  end
  
  #method to load file
  def load(synsets_file)
    
    line_num = 0
    invalid_lines = []
    correct_lines = {}
    nouns = []

    # Read from file
    File.open(synsets_file).each do |line|

      line_num += 1
      line.delete!("\n")
      
      # if format of line is correct, attempt to add it to corrects line hash
      if !!line.match(/^id:\s\d+\ssynset:\s[\w\.\'\/\-]+(\,[\w\.\'\/\-]+)*$/)
        words = line.split(Regexp.union(["id:","synset:",","," "])).reject { |c| c.empty? }

        id = Integer(words[0])
        nouns = words[1..words.size]

        # put invalid lines in an array and 
        # @synsets_hash has to be empty/nil at id
        if (id<0) or (@synsets_hash.has_key?(id)) or nouns.empty? or (correct_lines.has_key?(id))
          invalid_lines.append(line_num)

          # only store in correct_lines if invalid lines is empty
          # if it is not, hash should not store anything and return
          # and return invalid lines
        elsif invalid_lines.empty?
          correct_lines.store(id,nouns)
        end      
      else
        invalid_lines.append(line_num)
      end

    end

    #return 
    if not invalid_lines.empty?
      return invalid_lines
    else
      @synsets_hash.merge!(correct_lines)
      return nil
    end
  end

  # add synset method that returns false if id is negative,
  # list of nouns is empty, or the synset id has already been
  # defined without modifying the current object
  def addSet(synset_id, nouns)
    if (synset_id >= 0) and (not nouns.empty?) and (not @synsets_hash.has_key?(synset_id))
      @synsets_hash.store(synset_id,nouns)
      return true
    else
      return false    
    end
  end

  # lookup method
  def lookup(synset_id)
    if @synsets_hash.has_key?(synset_id)
      return @synsets_hash[synset_id]
    else
      return []
    end
  end

  #
  def findSynsets(to_find) 
    if String === to_find

      id_found = []

      @synsets_hash.each do |key,value|
        if value.include?(to_find)
          id_found.append(key)
        end
      end

      return id_found
      
    elsif Array === to_find and (not to_find.empty?)
      array_hash = {}

      # for each noun in the array, find if it exists
      # in the hash set, include the key in the
      # array_hash 
      to_find.each do |noun|
        #for each synset node, look if noun exists in 
        # value. If it does, add to the array_hash with
        # noun as the key and the synset key as the value
        @synsets_hash.each do |key,value|
          if value.include?(noun)
            if array_hash[noun] == nil
              array_hash[noun] = [key]
            elsif not array_hash[noun].empty?
              array_hash[noun].append(key)
            end

          end
        end
      end
      
      return array_hash
    else
      return nil
    end
  end
end




# Hypernyms Class #
class Hypernyms
  
  attr_accessor :hypernyms_hash

  def initialize
    @hypernyms_hash = {}
  end

  # load method
  def load(hypernyms_file)
    invalid_lines = []
    line_num = 0;
    correct_lines = {}

    ## Scan each line in file
    File.open(hypernyms_file).each do |line|
      line_num += 1
      
      line.delete!("\n") #remove leading new line characters

      
      if line.match(/^from:\s\d+\sto:\s\d+(\,[\d]+)*$/)
        # If format is correct, add the hypernym
        words = line.split(Regexp.union(["from:","to:",","," "])).reject { |c| c.empty? }

        
        source = Integer(words[0])
        hypernym_ids = words[1..words.size].map(&:to_i)
        
        if correct_lines[source] == nil and invalid_lines.empty?
          correct_lines[source] = hypernym_ids
        elsif correct_lines[source] and invalid_lines.empty?
          correct_lines[source] = correct_lines[source]|hypernym_ids
        end

      else
        # Get every invalid line
        invalid_lines.append(line_num)
      end
    end

    # Return
    if invalid_lines.empty?
      merge_hypernym(correct_lines)
      return nil
    end
    
    return invalid_lines
    
  end

  # add hypernym function
  def addHypernym(source, destination)
    if (destination == source) or (destination < 0) or (source < 0)
      return false
    elsif @hypernyms_hash[source] == nil
      @hypernyms_hash.store(source,[destination])
      return true
    elsif Array === @hypernyms_hash[source]
      if not @hypernyms_hash[source].include?(destination)
        @hypernyms_hash[source].append(destination)
      end
      return true
    end
    return false
  end

  # lowest common ancestor
  def lca(id1, id2)

    graph = Graph.new

    # Create the graph
    @hypernyms_hash.each do |key,value|
      
      # add vertices & edges
      if not graph.hasVertex?(key)
        graph.addVertex(key)
      end

      value.each do |destination|
        if not graph.hasVertex?(destination)
          graph.addVertex(destination)
        end
        graph.addEdge(key,destination)
      end

    end

    # check if the vertex exist
    if graph.hasVertex?(id1) and graph.hasVertex?(id2)
      if id1 == id2
        return [id1]
      else
        id1_bfs = graph.bfs(id1)
        id2_bfs = graph.bfs(id2)

        # all ancestors with: key = distance, value = [id]
        ancestors = {}

        id1_bfs.each do |key,value|
          if id2_bfs.has_key?(key)

            id2_bfs_value = id2_bfs[key]

            if ancestors[value+id2_bfs_value] == nil
              ancestors.store(value+id2_bfs_value,[key])
            else
              ancestors[value+id2_bfs_value].append(key)
            end
          end
        end

        # return the smallest key's value
        if ancestors.empty?
          return []
        else
          return ancestors.min_by{|k,v| k}[1]
        end

      end

    else
      return nil
    end
  end


  def merge_hypernym(temp_hypernym_hash)

    original_hash = @hypernyms_hash

    temp_hypernym_hash.each do |key,value|
      if original_hash.has_key?(key)
        original_hash[key] = original_hash[key] | value
      else # this means the key in temp does not exist in the original_hash
        original_hash[key] = value
      end
    end

    @hypernyms_hash = original_hash

    return true
  end

end

class CommandParser

  def initialize
    @synsets = Synsets.new
    @hypernyms = Hypernyms.new
  end

  def parse(command)

    response = {}
    command_arr = command.split(" ")

    #### start of switch case
    case command_arr[0]
    when /^load$/
      response[:recognized_command] = :load
      if (command_arr.size == 3) and (command[1].match(/^[\w\.\/\-]+$/)) and (command[2].match(/^[\w\.\/\-]+$/))
        load_result = load_syn_hyp(command_arr[1],command_arr[2])

        if load_result
          if merge_synset(load_result[0])
            merge_hypernym(load_result[1])
            response[:result] = true
          else
            response[:result] = false # merging the synsets didnt work
          end
        else
          response[:result] = false # new load method returned nil
        end

      else
        response[:result] = :error # number of arguments not correct
      end

    when /^lookup$/
      response[:recognized_command] = :lookup
      if command_arr.size == 2 and command_arr[1].match(/^\d+$/)
        response[:result] = @synsets.lookup(Integer(command_arr[1]))
      else
        response[:result] = :error
      end
      
    when /^find$/
      response[:recognized_command] = :find

      if command_arr.size == 2 and command_arr[1].match(/^[\w\/\-\.]+$/)
        response[:result] = @synsets.findSynsets(command_arr[1])
      else
        response[:result] = :error
      end

      
    when /^findmany$/
      response[:recognized_command] = :findmany
      if command_arr.size == 2 and command_arr[1].match(/^[\w\/\-\.]+(\,[\w\/\-\.]+)*$/)

        nouns = command_arr[1].split(",")
        response[:result] = @synsets.findSynsets(nouns)

      else
        response[:result] = :error
      end
      
    when /^lca$/
      response[:recognized_command] = :lca
      if command_arr.size == 3 and command_arr[1].match(/^\d+$/) and command_arr[2].match(/^\d+$/)
        id1 = Integer(command_arr[1])
        id2 = Integer(command_arr[2])
  
        response[:result] = @hypernyms.lca(id1,id2)

      else
        response[:result] = :error

      end

    else
      response[:recognized_command] = :invalid
    end

    return response

  end

  # Returns the synset_hash and hypernym_hash if
  # every synset involved in the hypernym_hash
  # is also in the synset_hash
  # otherwise false/nil
  def validate_hypernyms(synset_hash, hypernym_hash)

    hypernym_hash.each do |key,value|
      if not synset_hash.has_key?(key) and not @synset.synsets_hash.has_key?(key)
        return nil
      end

      value.each do |destination|
        if not synset_hash.has_key?(destination) and not @synset.synsets_hash.has_key?(destination)
          return nil
        end
      end
    end

    return synset_hash, hypernym_hash
  end

  # returns hashes from the temporary objects if valid
  # otherwise returns false
  def load_syn_hyp(synsets_file,hypernyms_file)
    
    if File.file?(synsets_file) and File.file?(hypernyms_file)
        #Files exist
        
        # create temp objects of hypernym and synset class     
      temp_hypernym = Hypernyms.new
      temp_synset = Synsets.new

      if not temp_synset.load(synsets_file) and not temp_hypernym.load(hypernyms_file)
        result = validate_hypernyms(temp_synset.synsets_hash,temp_hypernym.hypernyms_hash)
        if result != nil
          return result[0], result[1] # return synsets_hash and hypernym_hash if validation worked
        else
          return nil # return false if validation method is false
        end

      else
        return nil # return false if synsets_file/hypernym_file is invalid
      end
    else
      return nil # return false if any of the files do not exist
    end
   
  end

  # attempts to merge a temporary synset hash
  # to the main synset hash,
  # return nil if it hash common keys
  def merge_synset(temp_synset_hash)
    temp_synset_hash.each do |key,value|
      if @synsets.synsets_hash.has_key?(key)
        return nil
      end
    end
    @synsets.synsets_hash.merge!(temp_synset_hash)
    return temp_synset_hash
  end
  
  # merges one hypernym hash with the main one
  def merge_hypernym(temp_hypernym_hash)

    original_hash = @hypernyms.hypernyms_hash

    temp_hypernym_hash.each do |key,value|
      if original_hash.has_key?(key)
        original_hash[key] = original_hash[key] | value
      else # this means the key in temp does not exist in the original_hash
        original_hash[key] = value
      end
    end

    @hypernyms.hypernyms_hash = original_hash

    return true
  end
end
