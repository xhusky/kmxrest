package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.utils.KmxClientUtils;
import com.k2data.app.kmx.utils.OkhttpUtils;
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

    /**
     * 同步 get 请求
     *
     * @param cond 查询条件
     */
    @SuppressWarnings("unchecked")
    public static <T> T getSync(final KmxCond cond) {
        Map<String, String> params = cond.getParams();
        String url = cond.getUrl();

        try {
            logger.info(String.format("Kmx sync get. url: %s", url));
            if (logger.isDebugEnabled()) {
                logger.debug(String.format("params: %s", params));
            }

            Response response = OkhttpUtils.get(client, url, params);

            return (T) handleResponse(cond, url, params, response, null);
        } catch (IOException e) {
            throw new KmxException(String.format("Kmx get error! url: %s, params: %s", url, params.toString()), e);
        }
    }

    /**
     * 异步 get 请求
     *
     * @param cond 查询条件
     * @param handler 处理结果接口
     */
    @SuppressWarnings("unchecked")
    public static void getAsync(final KmxCond cond, final KmxResponseHandler handler) {
        final Map<String, String> params = cond.getParams();
        String url = cond.getUrl();

        OkhttpUtils.get(client, url, params, new Callback() {

            @Override
            public void onResponse(Call call, Response response) throws IOException {
                logger.info(String.format("Kmx async get callback. url: %s", url));
                if (logger.isDebugEnabled()) {
                    logger.debug(String.format("params: %s", params));
                }

                handleResponse(cond, url, params, response, handler);
            }

            @Override
            public void onFailure(Call call, IOException e) {
                logger.error(String.format("Kmx get error! url: %s", call.request().url().toString()), e);
            }
        });
    }

    /**
     * 同步 post 请求
     *
     * @param cond 查询条件
     */
    @SuppressWarnings("unchecked")
    public static <T> T postSync(final KmxCond cond) {
        String url = cond.getUrl();
        String raw = cond.getParams().get("query").replace("%2B", "+");

        try {
            logger.info(String.format("Kmx async post. url: %s", url));
            if (logger.isDebugEnabled()) {
                logger.debug(String.format("params: %s", raw));
            }

            Response response = OkhttpUtils.post(url, MediaType.parse("application/json"), raw);

            return (T) handleResponse(cond, url, raw, response, null);
        } catch (IOException e) {
            throw new KmxException(String.format("Kmx post error! url: %s, params: %s", url, raw), e);
        }
    }

    /**
     * 异步 post 请求
     *
     * @param cond 查询条件
     * @param handler 处理结果接口
     */
    @SuppressWarnings("unchecked")
    public static void postAsync(final KmxCond cond, final KmxResponseHandler handler) {
        String url = cond.getUrl();
        String raw = cond.getParams().get("query").replace("%2B", "+");

        OkhttpUtils.post(url, MediaType.parse("application/json"), raw, new Callback() {

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
                logger.error(String.format("Kmx get error! url: %s", call.request().url().toString()), e);
            }
        });
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
    private static Object handleResponse(KmxCond cond, String url, Map<String, String> params, Response response, KmxResponseHandler handler) throws IOException {
        return handleResponse(cond, url, params.toString(), response, handler);
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
