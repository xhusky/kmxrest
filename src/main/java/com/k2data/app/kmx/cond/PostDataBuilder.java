package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.Attribute;
import com.k2data.app.kmx.domain.Field;
import com.k2data.app.kmx.domain.FieldGroups;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.RequestType;
import com.k2data.app.kmx.utils.JsonUtils;
import com.k2data.app.kmx.utils.KmxClientUtils;

import java.util.*;
import java.util.stream.Collectors;

/**
 * fieldsGroup v2 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-19.
 */
public class PostDataBuilder extends KmxCondBuilder {

    private KmxInitParams initParams;

    public PostDataBuilder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    public PostBuilder post() {
        return new PostBuilder();
    }

    public class PostBuilder {
        private String fieldGroupId;
        private Long timestamp;
        private LinkedHashMap<String, Object> fields = new LinkedHashMap<>();

        public PostBuilder fieldGroupId(String fieldGroupId) {
            this.fieldGroupId = fieldGroupId;
            return this;
        }
        public PostBuilder timestamp(Long timestamp) {
            this.timestamp = timestamp;
            return this;
        }
        public PostBuilder addField(String field, Object value) {
            this.fields.put(field, value);
            return this;
        }
        public PostBuilder fields(LinkedHashMap<String, Object> fields) {
            this.fields = fields;
            return this;
        }
        public KmxCond build() {
            Map<String, String> params = new HashMap<>();
            params.put("query", JsonUtils.toJsonString(this));

            KmxCond kmxCond = new KmxCond();
            kmxCond.setUrl(initParams.getUrls().get(KmxCondType.postData));
            kmxCond.setParams(params);
            kmxCond.setClazz(String.class);
            kmxCond.setRequestType(RequestType.POST);
            kmxCond.setHandleResponse(false);

            return kmxCond;
        }
    }

}
