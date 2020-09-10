import {createLocalVue, shallowMount} from '@vue/test-utils'
import Notification from "../../src/components/Notification";
import dateTimeMixin from "../../src/mixins/dateTimeMixin";
import Vuex from 'vuex';

const localVue = createLocalVue();
localVue.use(Vuex);

const dataTemplate = {
    "message": "John Smith started following the activity 'Fun Run.'",
    "notificationType": 'ActivityFollowerAdded',
    "activityId": 20,
    "profileId": 0,
    "timeStamp": "2020-09-05T08:00:00+1300"
};

describe('Notification.vue', () => {
    const userId = 10;
    let store;
    let getters;

    beforeEach(() => {
        getters = {
            getUserId: () => userId
        };

        store = new Vuex.Store({
            getters
        });
    });

    it('Displays the message correctly with normal data', () => {
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: dataTemplate}
        });
        const message = wrapper.find("#message");
        expect(message.text()).toBe(dataTemplate.message);
    });

    it('Displays the date correctly with normal data', () => {
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: dataTemplate}
        });
        const date = wrapper.find("#date");
        expect(date.text()).toBe(dateTimeMixin.methods.dateFormat(dataTemplate.timeStamp));
    });

    it('Loads a self-triggered notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.profileId = userId;
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.borderColor).toBe("#ffffb0");
    });

    it('Loads an activity addition notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.notificationType = 'ActivityFollowerAdded';
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.borderColor).toBe("#99ff94");
    });

    it('Loads an activity removal notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.notificationType = 'ActivityFollowerRemoved';
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.borderColor).toBe("#f56e7a");
    });

    it('Loads an activity change notification with the right colour', () => {
        let data = {};
        Object.assign(data, dataTemplate);

        data.notificationType = 'ActivityEdited';
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.vm.$data.cardStyle.borderColor).toBe("#faf689");
    });
    it('Displays the view activity button if the notification has an activity ID', () => {
        let data = {};
        Object.assign(data, dataTemplate);
        data.activityId = 7
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.find("#viewButton").exists()).toBeTruthy();
    });
    it('Does not displays the view activity button if the notification does not have an activity ID', () => {
        let data = {};
        Object.assign(data, dataTemplate);
        data.activityId = null
        const wrapper = shallowMount(Notification, {
            store, localVue, propsData: {notification: data}
        });

        expect(wrapper.find("#viewButton").exists()).toBeFalsy();
    });

});