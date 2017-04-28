package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author lidong 17-1-17.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class PageInfo {

    private Integer pageNum;
    private Integer pageSize;
    private Integer size;
    private Integer total;
    private Integer pages;

    public Integer getPageNum() {
        return pageNum;
    }

    public void setPageNum(Integer pageNum) {
        this.pageNum = pageNum;
    }

    public Integer getPageSize() {
        return pageSize;
    }

    public void setPageSize(Integer pageSize) {
        this.pageSize = pageSize;
    }

    public Integer getSize() {
        return size;
    }

    public void setSize(Integer size) {
        this.size = size;
    }

    public Integer getTotal() {
        return total;
    }

    public void setTotal(Integer total) {
        this.total = total;
    }

    public Integer getPages() {
        return pages;
    }

    public void setPages(Integer pages) {
        this.pages = pages;
    }

}
