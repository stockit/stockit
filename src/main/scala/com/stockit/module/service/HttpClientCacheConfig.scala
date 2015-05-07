package com.stockit.module.service

import java.io.File

import scaldi.Module

/**
 * Created by dmcquill on 5/5/15.
 */
class HttpClientCacheConfig extends Module {

    bind [File] identifiedBy 'cacheDir to new File("cache")

}
