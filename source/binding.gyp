{
    "targets": [
        {
            "target_name": "SynchronousSocket",
            "sources": [ "SynchronousSocket.cc", "functions.cc" ],
            "include_dirs" : [
 	 			"<!(node -e \"require('nan')\")"
			]
        }
    ],
}
