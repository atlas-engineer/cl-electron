#include "functions.h"
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <iostream>

Nan::Persistent<v8::Function> SynchronousSocket::constructor;

NAN_MODULE_INIT(SynchronousSocket::Init) {
    v8::Local<v8::FunctionTemplate> tpl = Nan::New<v8::FunctionTemplate>(New);
    tpl->SetClassName(Nan::New("SynchronousSocket").ToLocalChecked());
    tpl->InstanceTemplate()->SetInternalFieldCount(1);

    Nan::SetPrototypeMethod(tpl, "connect", Connect);
    Nan::SetPrototypeMethod(tpl, "disconnect", Disconnect);
    Nan::SetPrototypeMethod(tpl, "read", Read);
    Nan::SetPrototypeMethod(tpl, "write", Write);

    constructor.Reset(Nan::GetFunction(tpl).ToLocalChecked());
    Nan::Set(target, Nan::New("SynchronousSocket").ToLocalChecked(), Nan::GetFunction(tpl).ToLocalChecked());
}

SynchronousSocket::SynchronousSocket(std::string socketPath) : socketPath_(socketPath) { }

SynchronousSocket::~SynchronousSocket() { }

NAN_METHOD(SynchronousSocket::New) {
    std::string socketPath = *Nan::Utf8String(info[0]);
    SynchronousSocket *obj = new SynchronousSocket(socketPath);
    obj->Wrap(info.This());
    info.GetReturnValue().Set(info.This());
}

NAN_METHOD(SynchronousSocket::Connect) {
    SynchronousSocket* obj = Nan::ObjectWrap::Unwrap<SynchronousSocket>(info.This());
    obj->socketfd_ = socket(AF_UNIX, SOCK_STREAM, 0);
    if (obj->socketfd_ == -1) {
        Nan::ThrowError("Unable to open socket file descriptor.");
    }
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, obj->socketPath_.c_str(), sizeof(addr.sun_path) - 1);
    if (connect(obj->socketfd_, (struct sockaddr*)&addr, sizeof(addr)) == -1) {
        close(obj->socketfd_);
        Nan::ThrowError("Unable to connect to socket.");
    }
}

NAN_METHOD(SynchronousSocket::Disconnect) {
    SynchronousSocket* obj = Nan::ObjectWrap::Unwrap<SynchronousSocket>(info.This());
    close(obj->socketfd_);
}

NAN_METHOD(SynchronousSocket::Read) {
    SynchronousSocket* obj = Nan::ObjectWrap::Unwrap<SynchronousSocket>(info.This());
    char buffer;
    ssize_t bytesRead;
    size_t bufferSize = 0;
    char* result = NULL;
    while ((bytesRead = read(obj->socketfd_, &buffer, 1)) > 0) {
        if (buffer == 0x04) {  // Ctrl+D (end of transmission)
            break;
        }
        char* temp = (char*)realloc(result, bufferSize + 1);
        if (temp == NULL) {
            free(result);
            return;
        }
        result = temp;
        result[bufferSize] = buffer;
        bufferSize++;
    }
    if (result != NULL) {
        result = (char*)realloc(result, bufferSize + 1);
        if (result != NULL) {
            result[bufferSize] = '\0';
        }
    }
    info.GetReturnValue().Set(Nan::New<v8::String>(result).ToLocalChecked());
}

NAN_METHOD(SynchronousSocket::Write) {
    SynchronousSocket* obj = Nan::ObjectWrap::Unwrap<SynchronousSocket>(info.This());
    if (!info[0]->IsString()) {
        return Nan::ThrowTypeError("Data must be a string.");
    }
    Nan::Utf8String socketDataArg(info[0]);
    const char* data = *socketDataArg;
    ssize_t size = strlen(data);
    if (write(obj->socketfd_, data, size) != size) {
        close(obj->socketfd_);
        Nan::ThrowError("Unable to write to socket.");
    }
}
