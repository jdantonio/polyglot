package engine
{
	public class Board
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
		
		public const DEFAULT_WIDTH:int = 7; 
		public const DEFAULT_HEIGHT:int = 6; 
		public const DEFAULT_NUM_TO_CONNECT:int = 4; 
		
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		private var _width:int;
		private var _height:int;
		private var _num_to_connect:int;
		
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
		 *   a player needs to connect in order to win the game
		 *   (default: 4).
		 */ 
		public function Board(width:int = DEFAULT_WIDTH, height:int = DEFAULT_HEIGHT, num_to_connect:int = DEFAULT_NUM_TO_CONNECT)
		{
			// set board width to a valid value
			if (width <= 0) {
				this._width = DEFAULT_WIDTH;
			} else {
				this._width = width;
			}

			// set board height to a valid value
			if (height <= 0) {
				this._height = DEFAULT_HEIGHT;
			} else {
				this._height = height;
			}
			
			// set board num_to_connect to a valid value
			// if the win condition is greater than width or height it is invalid
			// an invalid win condition is set to the minimum of width, height, or DEFAULT_NUM_TO_CONNECT
			if (num_to_connect <= 0 || num_to_connect > width || num_to_connect > height) {
				this._num_to_connect = Math.min(this._width, this._height);
				this._num_to_connect = Math.min(this._num_to_connect, DEFAULT_NUM_TO_CONNECT);
			} else {
				this._num_to_connect = num_to_connect;
			}
		}
		
		///////////////////////////////////////////////////////////////////////
		// Accessors
		
		/**
		 * @return The current width of the board.
		 */
		public function width():int
		{
			return this._width;
		}
		
		/**
		 * @return The current heigth of the board.
		 */
		public function height():int
		{
			return this._height;
		}
		
		/**
		 * @return The number of contiguous spaces needed to win.
		 */
		public function num_to_connect():int
		{
			return this._num_to_connect;
		}
	}
}