import {shallowMount, createLocalVue} from '@vue/test-utils'
import VueRouter from 'vue-router'
import {viewActivity} from "../../src/main";

const localVue = createLocalVue()
localVue.use(VueRouter)
const router = new VueRouter()

let wrapper;
let adminWrapper;

beforeEach(() => {
    wrapper = shallowMount(viewActivity, {
            localVue,
            router,
            computed: {
                hasShareAndEditPermissions() {
                    return false
                }
            },
            props: {idProp: 3}
        }
    );
    adminWrapper = shallowMount(viewActivity, {
            localVue,
            router,
            computed: {
                hasShareAndEditPermissions() {
                    return true
                }
            },
            props: {idProp: 3}
        }
    );


});

afterEach(() => {
    wrapper.destroy();
});

describe('ViewActivity.vue', () => {
    test('Share and edit buttons are not visible when user does not have admin permissions', () => {
        expect(wrapper.find("#shareButton").exists()).toBe(false)
        expect(wrapper.find("#editButton").exists()).toBe(false)
    });
});
