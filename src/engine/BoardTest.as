package engine
{
	import flexunit.framework.TestCase;

	public class BoardTest extends TestCase
	{
		public function testConstructionWithoutArgs():void
		{
			var board:engine.Board = new engine.Board();
			
			assertEquals(board.width(), 7);
			assertEquals(board.height(), 6);
			assertEquals(board.num_to_connect(), 4);
		}		

		public function testConstructionWithValidArgs():void
		{
			var board:engine.Board = new engine.Board(10, 7, 5);
			
			assertEquals(board.width(), 10);
			assertEquals(board.height(), 7);
			assertEquals(board.num_to_connect(), 5);
		}		
		
		public function testConstructionWithInvalidArgs():void
		{
			var board:engine.Board = new engine.Board(-1, 0, 1000);
			
			assertEquals(board.width(), 7);
			assertEquals(board.height(), 6);
			assertEquals(board.num_to_connect(), 4);
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
