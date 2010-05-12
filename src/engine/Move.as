package engine
{
	/**
	 * A bean class for storing one move. Intended to be used for history tracking.
	 * This is useful for replaying a game or cloning a board.
	 */
	public class Move
	{
		private var _color:int;
		private var _column:int;
		private var _timestamp:Date;

		public function Move(color:int, column:int)
		{
			this._color = color;
			this._column = column;
			this._timestamp = new Date();
		}

		public function get color():int
		{
			return this._color;
		}

		public function get column():int
		{
			return this._column;
		}

		public function get timestamp():Date
		{
			return this._timestamp;
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
