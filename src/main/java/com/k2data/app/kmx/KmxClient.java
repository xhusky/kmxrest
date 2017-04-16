package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.utils.KmxClientUtils;
import com.k2data.app.kmx.utils.OkHttpUtils;
import okhttp3.*;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * 连接大数据平台通用客户端
 *
 * @author lidong
 * @since 2016-7-26
 */
public class KmxClient {

    private static final Log logger = LogFactory.getLog(KmxClient.class);

    private static OkHttpClient client;
    static {
        Dispatcher dispatcher = new Dispatcher();
        dispatcher.setMaxRequests(20);
        dispatcher.setMaxRequestsPerHost(20);

        client = new OkHttpClient.Builder()
            .readTimeout(60, TimeUnit.SECONDS)      // 读取超时时间
            .writeTimeout(60, TimeUnit.SECONDS)     // 写超时时间
            .connectTimeout(60, TimeUnit.SECONDS)   // 连接超时时间
            .dispatcher(dispatcher)
            .build();
    }

    @SuppressWarnings("unchecked")
    public static <T> T sync(final KmxCond cond) {
        final String url = cond.getUrl();
        final Map<String, String> params = cond.getParams();
        final String raw = params.get("query").replace("%2B", "+");

        try {
            logger.info(String.format("Kmx sync request. %s, url: %s", cond.getRequestType().toString(), url));
            if (logger.isDebugEnabled()) {
                logger.debug(String.format("params: %s", raw));
            }

            Response response = null;
            switch (cond.getRequestType()) {
                case GET:
                    response = OkHttpUtils.get(client, url, params);
                    break;
                case POST:
                    response = OkHttpUtils.post(url, MediaType.parse("application/json"), raw);
                    break;
                case PUT:
                    response = OkHttpUtils.put(url, MediaType.parse("application/json"), raw);
                    break;
                case DELETE:
                    break;
            }

            return (T) handleResponse(cond, url, raw, response, null);
        } catch (IOException e) {
            throw new KmxException(String.format("Kmx request error! %s, url: %s, params: %s", cond.getRequestType().toString(), url, raw), e);
        }
    }

    public static void async(final KmxCond cond, final KmxResponseHandler handler) {
        final String url = cond.getUrl();
        final Map<String, String> params = cond.getParams();
        final String raw = params.get("query").replace("%2B", "+");;

        Callback callback = null;
        switch (cond.getRequestType()) {
            case GET:
                callback = new Callback() {

                    @Override
                    public void onResponse(Call call, Response response) throws IOException {
                        logger.info(String.format("Kmx async get callback. url: %s", url));
                        if (logger.isDebugEnabled()) {
                            logger.debug(String.format("params: %s", params));
                        }

                        handleResponse(cond, url, params.toString(), response, handler);
                    }

                    @Override
                    public void onFailure(Call call, IOException e) {
                        logger.error(String.format("Kmx get error! url: %s", call.request().url().toString()), e);
                    }
                };
                OkHttpUtils.get(client, url, params, callback);
                break;
            case POST:
                callback = new Callback() {

                    @Override
                    public void onResponse(Call call, Response response) throws IOException {
                        logger.info(String.format("Kmx async post callback. url: %s", url));
                        if (logger.isDebugEnabled()) {
                            logger.debug(String.format("raw: %s", raw));
                        }

                        handleResponse(cond, url, raw, response, handler);
                    }

                    @Override
                    public void onFailure(Call call, IOException e) {
                        logger.error(String.format("Kmx post error! url: %s", call.request().url().toString()), e);
                    }
                };
                OkHttpUtils.post(url, MediaType.parse("application/json"), raw, callback);
                break;
            case PUT:
                callback = new Callback() {

                    @Override
                    public void onResponse(Call call, Response response) throws IOException {
                        logger.info(String.format("Kmx async put callback. url: %s", url));
                        if (logger.isDebugEnabled()) {
                            logger.debug(String.format("raw: %s", raw));
                        }

                        handleResponse(cond, url, raw, response, handler);
                    }

                    @Override
                    public void onFailure(Call call, IOException e) {
                        logger.error(String.format("Kmx put error! url: %s", call.request().url().toString()), e);
                    }
                };
                OkHttpUtils.put(url, MediaType.parse("application/json"), raw, callback);
                break;
            case DELETE:
                break;
        }

        OkHttpUtils.get(client, url, params, callback);
    }

    /**
     * 处理 {@link Response}, 把json序列化成对象
     *
     * @param response 返回的{@link Response}
     * @param handler 异步调用的处理接口
     * @return 序列化后的对象
     * @throws IOException
     */
    @SuppressWarnings("unchecked")
    private static Object handleResponse(KmxCond cond, String url, String raw, Response response, KmxResponseHandler handler) throws IOException {
        String responseStr = response.body().string();

        if (KmxClientUtils.isBlank(responseStr)) {
            throw new KmxException(String.format("Request Kmx error, Response body is blank! Response code: %d, url: %s, params: %s",
                    response.code(),
                    url,
                    raw));
        }

        JSONObject rowsJson = JSON.parseObject(responseStr);

        if(!response.isSuccessful()) {
            throw new KmxException(String.format("Request Kmx error! response code: %d, Kmx code: %d, Kmx message: {%s}, url: %s, params: %s",
                    response.code(),
                    rowsJson.getInteger("code"),
                    rowsJson.getString("message"),
                    url,
                    raw));
        }

        if (rowsJson.getInteger("code") != 0) {
            if (logger.isWarnEnabled()) {
                logger.warn(String.format("Request Kmx error, code: %d, message: {%s}, url: %s, params: %s",
                        rowsJson.getInteger("code"),
                        rowsJson.getString("message"),
                        url,
                        raw));
            }
        }

        Object object = JSON.toJavaObject(rowsJson, cond.getClazz());

        if (handler != null) {
            handler.handleResponse(object);
        }

        return object;
    }

}
