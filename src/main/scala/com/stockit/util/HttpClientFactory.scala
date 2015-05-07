package com.stockit.util

import java.io.File

import org.apache.http.client.HttpClient
import org.apache.http.client.cache.HttpCacheContext
import org.apache.http.client.config.RequestConfig
import org.apache.http.conn.HttpClientConnectionManager
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.impl.client.cache.{CachingHttpClient, CacheConfig, CachingHttpClientBuilder, CachingHttpClients}

/**
 * Created by dmcquill on 3/25/15.
 */
class HttpClientFactory {

    def getHttpClient(cm: HttpClientConnectionManager): HttpClient = {
        HttpClients.custom.setConnectionManager(cm).build
    }

    def getFileCachingHttpClient(cm: HttpClientConnectionManager, cacheDir: File): HttpClient = {

        val cacheConfig = CacheConfig.custom()
            .setMaxCacheEntries(1000)
            .setMaxObjectSize(8192)
            .build()
        val requestConfig = RequestConfig.custom()
            .setConnectTimeout(30000)
            .setSocketTimeout(30000)
            .build()
        val cachingClient: CloseableHttpClient = CachingHttpClients.custom()
            .setCacheDir(cacheDir)
            .setCacheConfig(cacheConfig)
            .setDefaultRequestConfig(requestConfig)
            .build()

        cachingClient
    }

}
