#include "functions.h"

using v8::FunctionTemplate;

// C++ constructs that are exposed to JavaScript are exported here.

NAN_MODULE_INIT(InitAll) {
    SynchronousSocket::Init(target);
}

NODE_MODULE(SynchronousSocket, InitAll)
