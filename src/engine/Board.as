package engine
{
	public class Board
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
		
		public static const DEFAULT_WIDTH:int = 7; 
		public static const DEFAULT_HEIGHT:int = 6; 
		public static const DEFAULT_NUM_TO_CONNECT:int = 4; 
		
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		/** The number of pieces in a row needed to win. */
		private var _num_to_connect:int;
		
		/**
		 * A multi-dimensional array representing the board grid. The width
		 * represents the x-axis and the height represents the y-axis. Location
		 * (0,0) is the bottom left-hand corner. The value of each index
		 * indicates what piece is in the grid and must come from the enum
		 * GamePieceEnum. The array must be initialized to NONE in all indicies.
		 */
		private var _board:Array;
		
		/** The last player to take a turn as indicated by their piece color. */
		private var _last_player:int;
		
		/** The total number of possible winning sequences. */
		private var _num_of_win_places:int;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		/**
		 * Creates the board. The number of rows, number of columns, and win
		 * condition are configurable. When these values are not provided the
		 * board will default to the "7x6 win 4" rules of the basic game.
		 * 
		 * @param width The number of columns on the board (default: 7).
		 * @param height The number of rows on the board (default: 6).
		 * @param num_to_connect The number of contiguous board spaces
		 *   a player needs to connect in order to win the game (default: 4).
		 */ 
		public function Board(width:int = DEFAULT_WIDTH, height:int = DEFAULT_HEIGHT, num_to_connect:int = DEFAULT_NUM_TO_CONNECT)
		{
			// set last player to NONE since there has yet to be a move
			this._last_player = GamePieceEnum.NONE;
			
			// set board width to a valid value
			if (width <= 0) {
				width = DEFAULT_WIDTH;
			}
			
			// set board height to a valid value
			if (height <= 0) {
				height = DEFAULT_HEIGHT;
			}
			
			// set board num_to_connect to a valid value
			// if the win condition is greater than width or height it is invalid
			// an invalid win condition is set to the minimum of width, height, or DEFAULT_NUM_TO_CONNECT
			if (num_to_connect <= 0 || num_to_connect > width || num_to_connect > height) {
				this._num_to_connect = Math.min(width, height);
				this._num_to_connect = Math.min(this._num_to_connect, DEFAULT_NUM_TO_CONNECT);
			} else {
				this._num_to_connect = num_to_connect;
			}
			
			// create the board array
			this._board = new Array(width);
			for (var x:int = 0; x < width; x++)
			{
				this._board[x] = new Array(height)
				for (var y:int; y < height; y++)
				{
					this._board[x][y] = GamePieceEnum.NONE;
				}
			}
			
			// set initial player scores
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
		public function get num_to_connect():int
		{
			return this._num_to_connect;
		}
		
		public function get num_of_win_places():int
		{
			return Board.num_of_win_places(this.width, this.height, this._num_to_connect);
		}
		
		/**
		 * @return The color of the last player to make a move.
		 */
		public function get last_player():int
		{
			return this._last_player;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Utility Methods
		
		/**
		 * Calculate the number of possible winning combinations in a board of
		 * a given set of dimensions with a given number of pieces in a row
		 * required to win.
		 * 
		 * @note The algorithm of this function came directly from the source
		 *       Pomakis' game engine. There was no explanation of how this
		 *       worked. I am talking it on blind faith that it is correct.
		 * 
		 * @param width The width of the board.
		 * @param height The height of the board.
		 * @param num_to_connect The number of pieces in a row needed to win.
		 * 
		 * @return The number of possible winning combinations.
		 */
		public static function num_of_win_places(width:int, height:int, num_to_connect:int):int
		{
			if (width < num_to_connect && height < num_to_connect)
			{
				return 0;
			}
			else if (width < num_to_connect)
			{
				return width * ((height - num_to_connect) + 1);
			}
			else if (height < num_to_connect)
			{
				return height * ((width - num_to_connect) + 1);
			}
			else
			{
				return (4 * width * height)
					- (3 * width * num_to_connect)
					- (3 * height * num_to_connect)
					+ (3 * width)
					+ (3 * height)
					- (4 * num_to_connect)
					+ (2 * num_to_connect * num_to_connect)
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
