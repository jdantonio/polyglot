package engine
{
	import flexunit.framework.TestCase;
	
	public class BoardTest extends TestCase
	{
		public function testConstructionWithoutArgs():void
		{
			var board:engine.Board = new engine.Board();
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.win_condition, 4);
		}
		
		public function testConstructionWithValidArgs():void
		{
			var board:engine.Board = new engine.Board(10, 7, 5);
			
			assertEquals(board.width, 10);
			assertEquals(board.height, 7);
			assertEquals(board.win_condition, 5);
		}
		
		public function testConstructionWithInvalidArgs():void
		{
			var board:engine.Board = new engine.Board(-1, 0, 1000);
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.win_condition, 4);
		}
		
		public function testNumOfWinPlaces():void
		{
			// All of these tests values were manually counted
			// on a lined grid in Numbers. This isn't the best
			// way to create a test set but I could not find
			// a good reference.
			
			// This algorithm has four equivalence classes.
			// Each test below is for one of those classes.
			
			// invalid board values
			assertEquals(Board.num_of_win_places(5, 5, 10), 0);
			
			// width is less than number to connect
			assertEquals(Board.num_of_win_places(3, 6, 4), 9);
			
			// height is less than number to connect
			assertEquals(Board.num_of_win_places(6, 4, 5), 8);
			
			// standard game
			assertEquals(Board.num_of_win_places(7, 6, 4), 69);
			assertEquals((new Board(7, 6, 4)).num_of_win_places, 69);
		}
		
		public function testMagicWinNumber():void
		{
			assertEquals((new Board(10, 10, 1).magic_win_number), 2);
			assertEquals((new Board(10, 10, 2).magic_win_number), 4);
			assertEquals((new Board(10, 10, 3).magic_win_number), 8);
			assertEquals((new Board(10, 10, 4).magic_win_number), 16);
			assertEquals((new Board(10, 10, 5).magic_win_number), 32);
			assertEquals((new Board(10, 10, 6).magic_win_number), 64);
		}
		
		public function testDropPiece():void
		{
			var board:engine.Board = new engine.Board(7, 6, 4);
			
			// valid drops followed by column full
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 0);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 1);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 2);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 3);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 4);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), 5);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 0), Board.INVALID_MOVE);
			
			// invalid colums
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, -1), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(GamePieceEnum.BLACK, 7), Board.INVALID_MOVE);
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
