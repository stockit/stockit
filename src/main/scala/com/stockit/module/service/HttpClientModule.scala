package com.stockit.module.service

import java.io.File

import com.stockit.util.HttpClientFactory
import org.apache.http.client.HttpClient
import org.apache.http.conn.HttpClientConnectionManager
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import scaldi.Module

/**
 * Created by dmcquill on 3/26/15.
 */
class HttpClientModule extends Module {

    implicit val module = new HttpClientCacheConfig :: new HttpClientFactoryModule :: new HttpConnectionManagerModule

    bind [HttpClient] identifiedBy 'httpClient and 'poolingHttpClient to
        inject[HttpClientFactory]('httpClientFactory).getHttpClient(inject[HttpClientConnectionManager]('httpConnectionManager and 'poolingCM))

    bind [HttpClient] identifiedBy 'httpClient and 'poolingHttpClient and 'fileCache to inject[HttpClientFactory]('httpClientFactory).getFileCachingHttpClient(
            inject[HttpClientConnectionManager]('httpConnectionManager and 'poolingCM),
            inject[File]('cacheDir)
        )
}
