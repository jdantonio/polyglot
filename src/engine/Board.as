package engine
{	
	/**
	 * A virtual representation of a the game board. Virtual pieces can be placed
	 * into the game board by "dropping" them into columns. Like a real game
	 * board the virtual board only applies physical rules, not game rules. There
	 * is nothing preventing a player from taking multiple turns in a row (the
	 * board knows nothing about turns or play order), for example, but it will
	 * prevent additional pieces from being dropped into a full column. It is up
	 * to the Game class to manage game rules.
	 * 
	 * Unlike a physical game board the virtual board can be set to any size
	 * size so long as there are a positive number of rows and columns. The
	 * number of game pieces in a row needed to win is also configurable. Once
	 * the board is created, however, these settings cannot be changed.
	 * 
	 * One seemingly odd facet of the board is it is responsible for keeping
	 * score. On the surface it seems that score would be an attribute of the
	 * player but in reality a player's score is calculated based on the current
	 * state of the board, changes every time a piece is dropped into the board,
	 * and is algorithmically tied to the size of the board and the number of
	 * possible win conditions. 
	 */
	public class Board
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
		
		public static const DEFAULT_WIDTH:int = 7;
		public static const DEFAULT_HEIGHT:int = 6;
		public static const DEFAULT_WIN_CONDITION:int = 4;
		
		public static const INVALID_MOVE:int = -1;
		
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		/**
		 * A multi-dimensional array representing the board grid. The width
		 * represents the x-axis and the height represents the y-axis. Location
		 * (0,0) is the bottom left-hand corner. The value of each index
		 * indicates what piece is in the grid and must come from the enum
		 * Player. The array must be initialized to NONE in all indicies.
		 */
		private var _board:Array;
		
		/** The number of pieces in a row needed to win. */
		private var _win_condition:int;
		
		/** The total number of possible winning sequences. */
		private var _num_of_win_places:int;
		
		/** The scores and scores statistics of both platers. */
		private var _scores:Scores;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		/**
		 * Creates the board. The number of rows, number of columns, and win
		 * condition are configurable. When these values are not provided the
		 * board will default to the "7x6 win 4" rules of the basic game.
		 * 
		 * @param width The number of columns on the board (default: 7).
		 * @param height The number of rows on the board (default: 6).
		 * @param win_condition The number of contiguous board spaces
		 *   a player needs to connect in order to win the game (default: 4).
		 */ 
		public function Board(width:int = DEFAULT_WIDTH, height:int = DEFAULT_HEIGHT,
							  win_condition:int = DEFAULT_WIN_CONDITION)
		{
			// set board width to a valid value
			if (width <= 0) {
				width = DEFAULT_WIDTH;
			}
			
			// set board height to a valid value
			if (height <= 0) {
				height = DEFAULT_HEIGHT;
			}
			
			// set board win_condition to a valid value
			// if the win condition is greater than width or height it is invalid
			// an invalid win condition is set to the minimum of width, height,
			// or DEFAULT_WIN_CONDITION
			if (win_condition <= 0 || win_condition > width || win_condition > height) {
				this._win_condition = Math.min(width, height);
				this._win_condition = Math.min(this._win_condition, DEFAULT_WIN_CONDITION);
			} else {
				this._win_condition = win_condition;
			}
			
			// create the board array
			this._board = new Array(width);
			for (var x:int = 0; x < width; x++)
			{
				this._board[x] = new Array(height)
				for (var y:int; y < height; y++)
				{
					this._board[x][y] = Player.NONE;
				}
			}
			
			// create scores manager
			this._scores = new Scores(this);
		}
		
		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		/**
		 * @return The current width of the board.
		 */
		public function get width():int
		{
			return this._board.length;
		}
		
		/**
		 * @return The current height of the board.
		 */
		public function get height():int
		{
			return this._board[0].length;
		}
		
		/**
		 * @return The number of contiguous spaces needed to win.
		 */
		public function get win_condition():int
		{
			return this._win_condition;
		}
		
		public function get num_of_win_places():int
		{
			return Board.num_of_win_places(this.width, this.height, this._win_condition);
		}
		
		public function get winner():int
		{
			return this._scores.winner;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		/**
		 * Drop a piece of the specified color into the game board. Return
		 * the number of the landing row on success or INVALID_MOVE on failure.
		 * 
		 * As with a physical game board, the virtual Board allows the player
		 * to select a column and drop the piece into it. The piece then falls
		 * to the bottom-most open location. If the selected column is full
		 * then a piece cannot be dropped. As with a real game board there is
		 * nothing preventing a player from dropping multiple pieces in a
		 * given turn. Turns and alternating players are functions of the game
		 * and not the board. Similarly, there is nothing about a real game
		 * board that prevents a player from dropping foreign objects into the
		 * board. That, too, is a function of the game. Subsequently, the
		 * virtual board does not track player turns, it just accepts the piece
		 * it is given. It also does not check to see if the game has ended
		 * after the move. That is a state of the board and not an operation.
		 * It must be checked by the game code after a successful drop.
		 * 
		 * @pre The game_piece is a valid Player value.
		 * @pre The game piece belongs to the appropriate player.
		 * 
		 * @param game_piece The virtual game piece to be dropped into the board.
		 * @param column The column into which the piece is to be dropped.
		 * 
		 * @return The number of the landing row on success or INVALID_MOVE.
		 */
		public function drop_piece(player:int, column:int):int
		{
			// set a false return value
			var row:int = INVALID_MOVE;
			
			// check the color of the game piece
			if (Player.is_valid_color(player))
			{
				// make sure the column exists
				if (column >= 0 && column < this.width)
				{
					// iterate over the column
					for (var i:int = 0; i < this.height; i++)
					{
						// check for a valid drop location
						if (this._board[column][i] == Player.NONE)
						{
							this._board[column][i] = player;
							row = i;
							break;
						}
					}
				}
			}
			
			// update the scores
			if (row != INVALID_MOVE)
			{
				this._scores.update_score(player, column, row);
			}
			
			// return the row where the piece was dropped
			return row;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Utility Methods
		
		/**
		 * Calculate the number of possible winning combinations in a board of
		 * a given set of dimensions with a given number of pieces in a row
		 * required to win.
		 * 
		 * @param width The width of the board.
		 * @param height The height of the board.
		 * @param win_condition The number of pieces in a row needed to win.
		 * 
		 * @return The number of possible winning combinations.
		 */
		public static function num_of_win_places(width:int, height:int, win_condition:int):int
		{
			if (width < win_condition && height < win_condition)
			{
				return 0;
			}
			else if (width < win_condition)
			{
				return width * ((height - win_condition) + 1);
			}
			else if (height < win_condition)
			{
				return height * ((width - win_condition) + 1);
			}
			else
			{
				return (4 * width * height)
					- (3 * width * win_condition)
					- (3 * height * win_condition)
					+ (3 * width)
					+ (3 * height)
					- (4 * win_condition)
					+ (2 * win_condition * win_condition)
					+ 2;
			}
		}
	}
}
////////////////////////////////////////////////////////////////////////////////
// 
// Copyright (c) 2010 Jerry D'Antonio <stumpjumper@gmail.com>.
// 
// Licensed under MIT License http://www.opensource.org/licenses/mit-license.php
// 
////////////////////////////////////////////////////////////////////////////////
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.
// 
////////////////////////////////////////////////////////////////////////////////
