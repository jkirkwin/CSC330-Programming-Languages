# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

class MyPiece < Piece

  # class method to choose the next piece randomly
  def self.next_piece(board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # class method to create a single-cell "cheat" piece
  def self.cheat_piece(board)
    MyPiece.new([[[0, 0]]], board)
  end

  All_My_Pieces = All_Pieces + 
  [
    # 5-Long
    rotations([[0, 0], [1, 0], [-1, 0], [2, 0], [-2, 0]]),
    
    # Triangle
    rotations([[0, 0], [1, 0], [1, 1]]), 
    
    # Cleaver            
    rotations([[0, 0], [0, 1], [1, 0], [1, 1], [2, 1]]), 
  ] 
end

class MyBoard < Board
  
  # Create a new board using the custom pool of pieces
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self) 
    @score = 0
    @game = game
    @delay = 500
    @cheated = false
  end

  # Gets the next piece from the enhanced pool. May be a "cheat" piece if the 
  # cheat flag is set. Unsets the cheat flag. 
  def next_piece
    if @cheated
      @current_block = MyPiece.cheat_piece(self)
      @cheated = false
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled and updates 
  # the tick delay.
  def store_current
    locations = @current_block.current_rotation    
    displacement = @current_block.position

    (0..locations.size - 1).each{ |index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }

    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # rotates the current piece 180 degrees
  def flip
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheated?
    @cheated
  end

  # Reduces the score by 100 and ensures the next piece will be a "cheat"
  # piece. No effect if the cheated flag is set or if score is less than 100.
  def try_cheat
    if !cheated? && @score >= 100
      @score -= 100
      @cheated = true
    end
  end
end

class MyTetris < Tetris

  # Creates a canvas and the custom board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  # Extends parent class' bindings as required by the spec
  def key_bindings      
    # Add vanilla bindings
    super

    # Add 180 degree rotation binding
    @root.bind('u', proc { @board.flip })

    # Add binding to use cheat mechanism
    @root.bind('c', proc { @board.try_cheat })
  end
end