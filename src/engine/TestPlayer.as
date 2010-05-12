package engine
{
	public class TestPlayer extends Player
	{
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		private var _move_counter:int = -1;
		
		///////////////////////////////////////////////////////////////////////
		// Construction
		
		public function TestPlayer(color:int)
		{
			// call the parent constructor to remove abstract
			super(this, color);
		}
		
		///////////////////////////////////////////////////////////////////////
		// Game Operations
		
		public override function make_move():int
		{
			this._move_counter++;
			return this._move_counter;
		}
	}
}