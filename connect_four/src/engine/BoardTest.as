package engine
{
	import flexunit.framework.TestCase;
	
	import mx.effects.easing.Back;
	
	public class BoardTest extends TestCase
	{
		public function testConstructionWithoutArgs():void
		{
			var board:Board = new Board();
			
			assertEquals(board.width, 7);
			assertEquals(board.height, 6);
			assertEquals(board.win_condition, 4);
		}
		
		public function testConstructionWithValidArgs():void
		{
			var board:Board = new Board(10, 7, 5);
			
			assertEquals(board.width, 10);
			assertEquals(board.height, 7);
			assertEquals(board.win_condition, 5);
		}
		
		public function testConstructionWithInvalidArgs():void
		{
			var board:Board = new Board(-1, 0, 1000);
			
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
		
		public function testDropPieceVertical():void
		{
			// valid vertical drops followed by column full
			var board:Board = new Board();
			for (var i:int = 0; i < board.height; i++)
			{
				assertEquals(board.drop_piece(Player.BLACK, 0), i);
				assertEquals(board.drop_piece(Player.RED, 1), i);
			}
			assertEquals(board.drop_piece(Player.BLACK, 0), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(Player.RED, 1), Board.INVALID_MOVE);
		}
		
		public function testDropPieceHorizontal():void
		{
			// valid horizontal drops
			var board:Board = new Board();
			for (var i:int = 0; i < board.width; i++)
			{
				assertEquals(board.drop_piece(Player.BLACK, i), 0);
				assertEquals(board.drop_piece(Player.RED, i), 1);
			}
		}
		
		public function testDropPieceInvalidColumn():void
		{
			// invalid colums
			var board:Board = new Board();
			assertEquals(board.drop_piece(Player.RED, -1), Board.INVALID_MOVE);
			assertEquals(board.drop_piece(Player.BLACK, board.width), Board.INVALID_MOVE);
		}
		
		public function testNumSpaces():void
		{
			assertEquals(new Board(1, 1, 1).num_of_spaces, 1);
			assertEquals(new Board(3, 3, 1).num_of_spaces, 9);
			assertEquals(new Board(6, 7, 1).num_of_spaces, 42);
			assertEquals(new Board(6, 7, 9).num_of_spaces, 42);
			assertEquals(new Board(7, 6, 1).num_of_spaces, 42);
			assertEquals(new Board(7, 6, 9).num_of_spaces, 42);
		}
		
		public function testNumPiecesValidMoves():void
		{
			var board:Board = new Board(3, 3, 3);
			
			// empty board
			assertEquals(board.num_of_pieces, 0);

			// fill the board until a tie
			
			board.drop_piece(Player.BLACK, 0);
			assertEquals(board.num_of_pieces , 1);
			
			board.drop_piece(Player.RED, 1);
			assertEquals(board.num_of_pieces , 2);
			
			board.drop_piece(Player.BLACK, 0);
			assertEquals(board.num_of_pieces , 3);
			
			board.drop_piece(Player.RED, 1);
			assertEquals(board.num_of_pieces , 4);
			
			board.drop_piece(Player.BLACK, 2);
			assertEquals(board.num_of_pieces , 5);
			
			board.drop_piece(Player.RED, 0);
			assertEquals(board.num_of_pieces , 6);
			
			board.drop_piece(Player.BLACK, 2);
			assertEquals(board.num_of_pieces , 7);
			
			board.drop_piece(Player.RED, 1);
			assertEquals(board.num_of_pieces , 8);
			
			board.drop_piece(Player.BLACK, 2);
			assertEquals(board.num_of_pieces , 9);
		}
		
		public function testNumPiecesInalidMoves():void
		{
			var board:Board = new Board(2, 2, 2);
			
			// fill the first column and verify the number of pieces
			board.drop_piece(Player.BLACK, 0);
			board.drop_piece(Player.RED, 0);
			assertEquals(board.num_of_pieces , 2);
			
			// add more to the row and verify the count does not increment
			for (var i:int = 0; i < 5; i++)
			{
				board.drop_piece(Player.BLACK, 0);
				board.drop_piece(Player.RED, 0);
				assertEquals(board.num_of_pieces , 2);
			}
		}
		
		public function testIsTie():void
		{
			var board:Board = new Board(3, 3, 3);
			
			// fill the board until a tie
			
			board.drop_piece(Player.BLACK, 0);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.RED, 1);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.BLACK, 0);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.RED, 1);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.BLACK, 2);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.RED, 0);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.BLACK, 2);
			assertFalse(board.is_tie());
			
			board.drop_piece(Player.RED, 2);
			assertFalse(board.is_tie());
			
			// final piece should tie the game
			board.drop_piece(Player.BLACK, 1);
			assertTrue(board.is_tie());
		}
		
		public function testSpaceColorAndOpenness():void
		{
			var board:Board = new Board(3, 3, 3);

			// check the full board for empty
			for (var x:int = 0; x < board.width; x++)
			{
				for (var y:int = 0; y < board.height; y++)
				{
					assertEquals(board.color_of_space(x, y), Player.NONE);
					assertTrue(board.is_space_open(x, y));
				}
			}
			
			// drop a few pieces and check
			
			board.drop_piece(Player.RED, 0);
			assertEquals(board.color_of_space(0, 0), Player.RED);
			assertFalse(board.is_space_open(0, 0));
			
			board.drop_piece(Player.BLACK, 0);
			assertEquals(board.color_of_space(0, 0), Player.RED);
			assertFalse(board.is_space_open(0, 0));
			assertEquals(board.color_of_space(0, 1), Player.BLACK);
			assertFalse(board.is_space_open(0, 1));
		}
		
		public function testIsColumnOpen():void
		{
			var board:Board = new Board(3, 3, 3);
			
			// check that all columns are open
			for (var x:int = 0; x < board.width; x++)
			{
				assertTrue(board.is_column_open(x));
			}
			
			// fill a row and check it
			for (var y:int = 0; y < board.height - 1; y++)
			{
				board.drop_piece(Player.RED, 0);
				assertTrue(board.is_column_open(0));
			}
			board.drop_piece(Player.RED, 0);
			assertFalse(board.is_column_open(0));
		}
		
		public function testEquals():void
		{
			var b1:Board = new Board();
			var b2:Board = new Board();
			
			// check new boards and null
			assertFalse(b1.equals(null));
			assertTrue(b1.equals(b2));
			
			// make a few moves and check again
			for (var x:int = 0; x < b1.width; x++)
			{
				b1.drop_piece(Player.BLACK, x);
				b2.drop_piece(Player.BLACK, x);
				b1.drop_piece(Player.RED, x);
				b2.drop_piece(Player.RED, x);
			}
			assertTrue(b1.equals(b2));
			
			// make another move to break equality
			b1.drop_piece(Player.BLACK, 0);
			assertFalse(b1.equals(b2));
			
			// create two different boards and compare
			b1 = new Board(7, 6, 4);
			b2 = new Board(6, 7, 4);
			assertFalse(b1.equals(b2));
		}
		
		public function testClone():void
		{
			var b1:Board, b2:Board;

			// clone and empty board and compare
			b1 = new Board();
			b2 = Board.clone(b1);
			assertTrue(b1.equals(b2));
			
			// make a few moves then clone and compare
			b1 = new Board(3, 3, 3);
			b1.drop_piece(Player.BLACK, 0);
			b1.drop_piece(Player.RED, 0);
			b1.drop_piece(Player.BLACK, 2);
			b1.drop_piece(Player.RED, 3);
			b2 = Board.clone(b1);
			assertTrue(b1.equals(b2));
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
