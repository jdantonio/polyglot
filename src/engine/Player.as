package engine
{
	import flash.errors.IllegalOperationError;
	
	/**
	 * An abstract class representing one of the two players in the game. This
	 * class should be extended by each user interface built on top of the engine.
	 * 
	 * ActionScript does not support true abstract classes. The technique used
	 * by this class to to prevent instanciation was developed by Josh Tynjala
	 * and posted to his blog.
	 * 
	 * @link http://joshblog.net/2007/08/19/enforcing-abstract-classes-at-runtime-in-actionscript-3/
	 */
	public class Player
	{
		///////////////////////////////////////////////////////////////////////
		// Constants
		
		public static const NONE:int = 0;
		public static const RED:int = 1;
		public static const BLACK:int = 2;
		
		///////////////////////////////////////////////////////////////////////
		// Data Members
		
		private var _color:int;
		
		private var _board:Board;
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		/**
		 * @param self Enforces the anstractness of the class. A reference to
		 *        "this" must be passed to the super constructor of all classes
		 *        which extend Player.
		 * @param board The board on which this player is playing.
		 * @param color The color of playing piece this player is using.
		 */
		public function Player(self:Player, board:Board, color:int)
		{
			// validate parameters
			if (self != this) {
				throw new IllegalOperationError("Player cannot be instantiated directly.");
			} else if (! Player.is_valid_color(color)) {
				throw new TypeError("Player must be instantiated with a valid color.");
			} else if (board == null) {
				throw new TypeError("Player must be instantiated with a valid board.");
			}
			
			// set the internal data
			this._color = color;
			this._board = board;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		/** The color of playing piece this player is using. */
		public function get color():int
		{
			return this._color;
		}
		
		/** The board on which this player is playing. */
		protected function get board():Board
		{
			return this._board;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations

		/**
		 * Examine the board and select a next move. Must be overridden by all
		 * classes extending Player in order to participate in the game.
		 * 
		 * @return The column in which the next piece should be placed. Always
		 *         Board.INVALID_MOVE unless overridden.
		 */
		public function make_move():int
		{
			return Board.INVALID_MOVE;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Enum Functions
		
		/**
		 * Determine if a given integer is one of the two allowed color constants.
		 * 
		 * @param color The integer being checked.
		 * 
		 * @return True if color is a valid color else false.
		 * 
		 * @see is_valid
		 */
		public static function is_valid_color(color:int):Boolean
		{
			if (color == RED || color == BLACK) {
				return true;
			} else {
				return false;
			}
		}
		
		/**
		 * ActionScript does not support proper enumerations so there is always
		 * a possibility of a non-color integer being passed when a color is
		 * intended. This method validates whether a given integer is a
		 * valid value for color. The non-color NONE constant is considered
		 * valid.
		 * 
		 * @param color The integer being checked.
		 * 
		 * @return True if color is valid else false.
		 */
		public static function is_valid(color:int):Boolean
		{
			if (color == RED || color == BLACK || color == NONE) {
				return true;
			} else {
				return false;
			}
		}
		
		/**
		 * Give one color return the other. Similar to logical negation.
		 * 
		 * @param color The integer being checked.
		 * 
		 * @return The other color else NONE if color is not valid.
		 */
		public static function other_color(color:int):int
		{
			if (color == RED) {
				return BLACK;
			} else if (color == BLACK) {
				return RED;
			} else {
				return NONE;
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
