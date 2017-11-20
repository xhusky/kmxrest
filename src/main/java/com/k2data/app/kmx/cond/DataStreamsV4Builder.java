package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.domain.DataStreams;
import com.k2data.app.kmx.enums.*;
import com.k2data.app.kmx.utils.Assert;

import com.k2data.app.kmx.utils.KmxClientUtils;
import java.util.*;

/**
 * data-streams v0.4.0 查询条件 builder, 可链式调用添加条件, 最后调用 {@code build()} 生成查询条件
 *
 * @author lidong 17-1-16.
 */
public class DataStreamsV4Builder extends KmxCondBuilder {

    private KmxInitParams initParams;

    private String fieldGroup;
    private String start;
    private String end;
    private Set<String> fields = new HashSet<>();

    private List<String> valueFilters = new ArrayList<>();
    private List<String> valueTrans = new ArrayList<>();

    private String idValue;
    private List<String> orIdValue = new ArrayList<>();
    private List<String> andIdValue = new ArrayList<>();

    private List<String> aggregations = new ArrayList<>();
    private String interval;
    private Boolean naturalTimeBoundary;
    private Object fill;

    private List<String> order = new ArrayList<>();   // 排序
    private Integer size;    // 每页大小 ，注:针对实时接口该字段为每个设备显示的最近点数
    private Integer page;    // 第几页

    private String resultFormatIso;

    private RequestType requestType = RequestType.POST;

    public DataStreamsV4Builder(KmxInitParams initParams) {
        this.initParams = initParams;
    }

    /**
     * 生成查询条件
     *
     * @return 查询条件 json
     */
    public KmxCond build() {
        Assert.notEmpty(fields, "Field must not be null");
        Assert.notNull(start, "Start must not be null");
        Assert.notNull(end, "End must not be null");

        String paramsSb = "{" + noSignList(
                objField("fieldGroup", fieldGroup),
                list("fields", new ArrayList<>(fields), true),
                obj("timeRange",
                        objField("start", start),
                        objField("end", end)
                ),
                list("valueFilters", valueFilters),
                list("valueTrans", valueTrans),
                obj("coValueFilter",
                        obj("idFieldFilter",
                                obj(initParams.getIdField(), objField(Sign.EQ.getValue(), idValue)),
                                list(Sign.OR.getValue(), orIdValue),
                                list(Sign.AND.getValue(), andIdValue)
                        )
                ),
                list("aggregations", aggregations),
                obj("aggregationOptions",
                        objField("interval", interval),
                        objField("naturalTimeBoundary", naturalTimeBoundary),
                        objField("fill", fill)
                ),
                list("order", order),
                objField("size", size),
                objField("page", page),
                obj("options", objField("resultTimeFormat", resultFormatIso))
        ) + "}";

        Map<String, String> params = new HashMap<>();
        params.put("query", paramsSb);

        KmxCond cond = new KmxCond();
        cond.setUrl(initParams.getUrls().get(KmxCondType.dataStreams));
        cond.setParams(params);
        cond.setClazz(DataStreams.class);
        cond.setRequestType(requestType);

        return cond;
    }

    public DataStreamsV4Builder post() {
        this.requestType = RequestType.POST;
        return this;
    }

    public DataStreamsV4Builder get() {
        this.requestType = RequestType.GET;
        return this;
    }

    public DataStreamsV4Builder fieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
        return this;
    }

    public DataStreamsV4Builder start(Date start) {
        this.start = KmxClientUtils.format(start);
        return this;
    }

    public DataStreamsV4Builder start(String start) {
        this.start = start;
        return this;
    }

    public DataStreamsV4Builder end(Date end) {
        this.end = KmxClientUtils.format(end);
        return this;
    }

    public DataStreamsV4Builder end(String end) {
        this.end = end;
        return this;
    }

    /* fields begin */
    public DataStreamsV4Builder fields(Set<String> fields) {
        this.fields = fields;
        return this;
    }

    public DataStreamsV4Builder fields(List<String> fields) {
        this.fields = new HashSet<>(fields);
        return this;
    }

    public DataStreamsV4Builder fields(String[] fields) {
        Collections.addAll(this.fields, fields);
        return this;
    }

    public DataStreamsV4Builder field(String field) {
        this.fields.add(field);
        return this;
    }
    /* fields end */

    public DataStreamsV4Builder idValue(String idValue) {
        this.idValue = idValue;
        return this;
    }

    public DataStreamsV4Builder orIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataStreamsV4Builder andIdValue(String field, String value) {
        this.orIdValue.add(String.format("{ \"%s\": { \"$eq\": \"%s\" } }", field, value));
        return this;
    }

    public DataStreamsV4Builder valueFilters(List<String> valueFilters) {
        this.valueFilters = valueFilters;
        return this;
    }

    public DataStreamsV4Builder valueFilters(String valueFilters) {
        this.valueFilters.add(valueFilters);
        return this;
    }

    public DataStreamsV4Builder valueTrans(List<String> valueTrans) {
        this.valueTrans = valueTrans;
        return this;
    }

    public DataStreamsV4Builder valueTrans(String key, String value) {
        this.valueTrans.add("{ \"" + key + "\": " + "\"" + value + "\" }");
        return this;
    }

    public DataStreamsV4Builder valueTrans(String valueTrans) {
        this.valueTrans.add(valueTrans);
        return this;
    }

    public DataStreamsV4Builder aggregations(String aggregations) {
        this.aggregations.add(aggregations);
        return this;
    }

    public DataStreamsV4Builder aggregations(Aggregation... aggregations) {
        this.aggregations.add(buildAggregations("$default", aggregations));
        return this;
    }

    public DataStreamsV4Builder aggregations(String field, Aggregation... aggregations) {
        this.aggregations.add(buildAggregations(field, aggregations));
        return this;
    }

    public DataStreamsV4Builder aggregations(String field, String name, Aggregation aggregation) {
        this.aggregations.add(String.format("{ \"%s\": {\"type\": \"%s\",\"name\": \"%s\"} }",
                field,
                aggregation.getLowerValue(),
                name));
        return this;
    }

    private String buildAggregations(String field, Aggregation... aggregations) {
        StringBuilder sb = new StringBuilder();
        sb.append("{ \"").append(field).append("\": [");
        for (int i = 0; i < aggregations.length; i++) {
            Aggregation aggregation = aggregations[i];

            if (i != 0) {
                sb.append(",");
            }

            sb.append("\"").append(aggregation.getLowerValue()).append("\"");
        }
        sb.append("] }");

        return sb.toString();
    }

    public DataStreamsV4Builder interval(String interval) {
        this.interval = interval;
        return this;
    }

    public DataStreamsV4Builder naturalTimeBoundary(boolean naturalTimeBoundary) {
        this.naturalTimeBoundary = naturalTimeBoundary;
        return this;
    }

    public DataStreamsV4Builder fill(Object fill) {
        this.fill = fill;
        return this;
    }

    public DataStreamsV4Builder order(String field, Order order) {
        this.order.add(String.format("{ \"%s\": \"%s\" }", field, order.getLowerValue()));
        return this;
    }

    public DataStreamsV4Builder order(Order order) {
        this.order.add(String.format("{ \"$default\": \"%s\" }", order.getLowerValue()));
        return this;
    }

    public DataStreamsV4Builder size(Integer size) {
        this.size = size;
        return this;
    }

    public DataStreamsV4Builder page(Integer page) {
        this.page = page;
        return this;
    }

    public DataStreamsV4Builder resultFormatIso() {
        this.resultFormatIso = "iso";
        return this;
    }

}
