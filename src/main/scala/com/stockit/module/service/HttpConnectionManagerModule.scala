package com.stockit.module.service

import org.apache.http.conn.HttpClientConnectionManager
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import scaldi.Module

/**
 * Created by dmcquill on 5/5/15.
 */
class HttpConnectionManagerModule extends Module {

    bind [HttpClientConnectionManager] identifiedBy 'httpConnectionManager and 'poolingCM to new PoolingHttpClientConnectionManager() initWith {
        _.setDefaultMaxPerRoute(10)
    } initWith {
        _.setMaxTotal(10)
    }
}
