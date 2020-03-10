# Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in,
# so do not modify the other files as
# part of your solution.

#########################################################
# TODO - Add cheat piece and associated functionality
#########################################################

class MyPiece < Piece

  # class method to choose the next piece randomly
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
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
  end

  # Gets the next piece from the enhanced pool
  def next_piece
    @current_block = MyPiece.next_piece(self)
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

  def flip
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
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

  # Extends parent class' bindings as specified
  def key_bindings      
    # Add vanilla bindings
    super

    # Add 180 degree rotation binding
    @root.bind('u', proc { @board.flip })
    end
end