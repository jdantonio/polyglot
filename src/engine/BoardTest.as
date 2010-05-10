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