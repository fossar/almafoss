var _fossar$almafoss$Native_Scroll = function() {

var fakeNode = {
	getElementById: function() { return null; },
	addEventListener: function() {},
	removeEventListener: function() {}
};

var doc = (typeof document !== 'undefined') ? document : fakeNode;

var rAF = typeof requestAnimationFrame !== 'undefined'
	? requestAnimationFrame
	: function(callback) { callback(); };

function withNode(id, doStuff) {
	return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
		rAF(function() {
			var node = doc.getElementById(id);
			if (node === null) {
				callback(_elm_lang$core$Native_Scheduler.fail({ ctor: 'NotFound', _0: id }));
				return;
			}
			callback(_elm_lang$core$Native_Scheduler.succeed(doStuff(node)));
		});
	});
}


function intoView(id) {
	return withNode(id, function(node) {
		node.scrollIntoView();
		return _elm_lang$core$Native_Utils.Tuple0;
	});
}


return {
	intoView: intoView,
};

}();
