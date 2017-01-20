package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.AssetsDomain;
import com.k2data.app.kmx.enums.KmxCondType;

import java.util.HashMap;
import java.util.Map;

/**
 * assets v0.4.0 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-19.
 */
public class AssetsBuilder extends KmxCondBuilder {

    private KmxInitParams initParams;
    private Map<String, String> params = new HashMap<>();

    public AssetsBuilder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    @Override
    public KmxCond build() {
        KmxCond kmxCond = new KmxCond();
        kmxCond.setUrl(initParams.getUrls().get(KmxCondType.assets));
        kmxCond.setParams(params);
        kmxCond.setClazz(AssetsDomain.class);

        return kmxCond;
    }

    /**
     * 添加 get 请求参数
     *
     * @param key
     * @param value
     * @return
     */
    public AssetsBuilder addParams(String key, String value) {
        params.put(key, value);
        return this;
    }

}
