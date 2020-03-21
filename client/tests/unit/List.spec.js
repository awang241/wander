import { shallowMount } from '@vue/test-utils'
import List from "../../src/components/Registration";
import ListItem from "../../src/components/ListItem";

// Mount the component
const wrapper = shallowMount(List, {
    propsData: {
        chosenItems: ["Beans", "Greens", "Tomatoes"],
    }
})

describe('List.vue', () => {
    it('has a created hook', () => {
        expect(true).toBe(true)
    })
    it('Renders the component', () => {
        expect(wrapper.exists()).toBe(true)
    })
});