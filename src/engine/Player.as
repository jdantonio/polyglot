package engine
{
	import flash.errors.IllegalOperationError;
	
	/**
	 * An abstract class
	 * 
	 * http://joshblog.net/2007/08/19/enforcing-abstract-classes-at-runtime-in-actionscript-3/
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
		
		///////////////////////////////////////////////////////////////////////
		// Construction

		public function Player(self:Player, color:int)
		{
			// validate parameters
			if (self != this) {
				throw new IllegalOperationError("Player cannot be instantiated directly.");
			} else if (! Player.is_valid_color(color)) {
				throw new TypeError("Player must be instantiated with a valid color.");
			}
			
			// set the color
			this._color = color;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Property Accessors
		
		public function get color():int
		{
			return this._color;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		public function make_move():int
		{
			return Board.INVALID_MOVE;
		}
		
		///////////////////////////////////////////////////////////////////////
		// Enum Functions
		
		public static function is_valid_color(color:int):Boolean
		{
			if (color == RED || color == BLACK) {
				return true;
			} else {
				return false;
			}
		}
		
		public static function is_valid(color:int):Boolean
		{
			if (color == RED || color == BLACK || color == NONE) {
				return true;
			} else {
				return false;
			}
		}
		
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
