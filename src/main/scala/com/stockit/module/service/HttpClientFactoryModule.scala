package com.stockit.module.service

import com.stockit.util.HttpClientFactory
import scaldi.Module

/**
 * Created by dmcquill on 5/5/15.
 */
class HttpClientFactoryModule extends Module {

    bind [HttpClientFactory] identifiedBy 'httpClientFactory to new HttpClientFactory
}
